%%
%% hash_reader.erl
%% Kevin Lynx
%% 06.28.2013
%%
-module(hash_reader).
-compile(export_all).
-include("vlog.hrl").
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1,
		 stop/1]).
-record(state, {downloader, dbpool, downloading = 0}).
-include("db_common.hrl").
% if there's no hash, wait some time
-define(WAIT_TIME, 1*60*1000).
% the max concurrent download tasks
-define(MAX_DOWNLOAD, 50).
-define(DOWNLOAD_INTERVAL, 100).

start_link(DBPool) ->
	gen_server:start_link(?MODULE, [DBPool], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

% 
init([DBPool]) ->
	{ok, DownPid} = tor_download:start_link(),
	tor_download_stats:register(DownPid),
	{ok, #state{dbpool = DBPool, downloader = DownPid}, 0}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(filter_torrent, State) ->
	Conn = db_conn(State),
	try_next_download(Conn),
	{noreply, State};

handle_info({got_torrent, failed, _Hash}, State) ->
	#state{downloading = D} = State,
	Conn = db_conn(State),
	try_next_download(Conn),
	?T(?FMT("got torrent failed ~s", [_Hash])),
	hash_reader_stats:handle_download_failed(),
	{noreply, State#state{downloading = D - 1}};

handle_info({got_torrent, ok, Hash, Content}, State) ->
	hash_reader_stats:handle_download_ok(),
	Conn = db_conn(State),
	true = is_binary(Content),
	% save the torrent file
	?T(?FMT("got torrent ok ~s size ~p", [Hash, byte_size(Content)])),
	SaveTor = config:get(save_torrent, true),
	if SaveTor -> loc_torrent_cache:save(Conn, Hash, Content); true -> ok end,
	NewState = got_torrent_content(Conn, State, Hash, Content),
	{noreply, NewState};

handle_info({got_torrent_from_cache, Hash, Content}, State) ->
	Conn = db_conn(State),
	NewState = got_torrent_content(Conn, State, Hash, Content),
	{noreply, NewState};

handle_info(timeout, State) ->
	Conn = db_conn(State),
	?T("handle timeout, try next"),
	try_next(Conn),
	{noreply, State};

% when there's no hash to process
handle_info(process_download_hash, State) ->
	#state{downloading = D} = State,
	NewD = case D >= ?MAX_DOWNLOAD of
		true ->
			% the only thing we can do is just wait
			timer:send_after(?WAIT_TIME, timeout),
			?T(?FMT("reach the max download ~p", [D])),
			D;
		false ->
			?T(?FMT("start a new download ~p", [D])),
			% launch downloader
			Conn = db_conn(State),
			case try_next_download(Conn) of
				ok ->
					% until the max downloader count reaches
					timer:send_after(?DOWNLOAD_INTERVAL, process_download_hash);
				empty ->
					skip
			end,
			D % + 1, bug here ?
	end,
	{noreply, State#state{downloading = NewD}};

handle_info(M, State) ->
	?W(?FMT("unhandled message ~p", [M])),
	{noreply, State}.

handle_cast({process_hash, Doc,  DownloadDoc}, State) ->
	Conn = db_conn(State),
	{Hash} = bson:lookup(hash, Doc),
	ReqCnt = get_req_cnt(Doc), 
	ListHash = binary_to_list(Hash),
	?T(?FMT("process a hash ~s download-doc ~p", [ListHash, DownloadDoc])),
	% to avoid register many timers when the hash is empty but download hash is not
	% it also can avoid increase the message queue size, everytime this function get called,
	% it remove this message and append only another 1.
	if DownloadDoc -> do_nothing; true -> try_next(Conn) end,
	NewState = case db_store_mongo:inc_announce(Conn, ListHash, ReqCnt) of
		true -> 
			?T(?FMT("inc_announce success ~s", [ListHash])),
			on_updated(Conn),
			if DownloadDoc -> try_next_download(Conn); true -> ok end,
			State;
		false ->
			?T(?FMT("start to download the torrent ~s", [ListHash])),
			try_download(State, ListHash, Doc)
	end,
	{noreply, NewState};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

get_req_cnt(Doc) ->
	case bson:lookup(req_cnt, Doc) of
		{} -> 0;
		{R} -> R
	end.

try_download(State, Hash, Doc) ->
	#state{downloading = D} = State,
	NewDownloading = case D >= ?MAX_DOWNLOAD of
		true -> % put it into the download queue
			?T(?FMT("reach the max download, insert it to wait queue ~s", [Hash])),
			Conn = db_conn(State),
			insert_to_download_wait(Conn, Doc),
			D;
		false -> % download it now
			do_download(State, Hash),
			D + 1
	end,
	State#state{downloading = NewDownloading}.

% and now we can retrieve the torrent from local cache
do_download(State, Hash) ->
	#state{downloader = Pid} = State,
	Conn = db_conn(State),
	case loc_torrent_cache:load(Conn, Hash) of
		not_found -> % not in the local cache, download it now
			?T(?FMT("push a request to tor_download ~s", [Hash])),
			tor_download:download(Pid, Hash);
		Content -> % process later
			?T(?FMT("load a torrent from cache ~s", [Hash])),
			on_used_cache(),
			self() ! {got_torrent_from_cache, Hash, Content}
	end.

try_save(State, Hash, Name, Length, Files) ->
	Conn = db_conn(State),
	case catch db_store_mongo:insert(Conn, Hash, Name, Length, Files) of
		{'EXIT', Reason} ->
			?E(?FMT("save torrent failed ~p", [Reason]));
		_ -> 
			on_saved(Conn)
	end.

on_used_cache() ->
	hash_reader_stats:handle_used_cache().

on_saved(Conn) ->
	% `get_peers' here means we have processed a request
	db_system:stats_get_peers(Conn),
	% increase the `new' counter
	db_system:stats_new_saved(Conn),
	hash_reader_stats:handle_insert().

on_updated(Conn) ->
	% `get_peers' here means we have processed a request
	db_system:stats_get_peers(Conn),
	% also increase the updated counter
	db_system:stats_updated(Conn),
	hash_reader_stats:handle_update().

got_torrent_content(Conn, State, MagHash, Content) ->
	#state{downloading = D} = State,
	try_next_download(Conn),
	case catch(torrent_file:parse(Content)) of
		{'EXIT', _} ->
			?W(?FMT("parse a torrent failed ~s", [MagHash])),
			skip;
		{Type, Info} -> 
			got_torrent(State, MagHash, Type, Info)
	end,
	State#state{downloading = D - 1}.

got_torrent(State, Hash, single, {Name, Length}) ->
	try_save(State, Hash, Name, Length, []);

got_torrent(State, Hash, multi, {Root, Files}) ->
	try_save(State, Hash, Root, 0, Files).

% insert the doc to the `wait-download' collection, and when the 
% downloader is free, it will download this doc.
insert_to_download_wait(Conn, Doc) ->
	{ID} = bson:lookup('_id', Doc),
	Sel = {'_id', ID},
	mongo:do(safe, master, Conn, ?HASH_DBNAME, fun() ->
		% may exist already
		mongo:update(?HASH_DOWNLOAD_COLL, Sel, Doc, true)
	end).

try_next_download(Conn) ->
	Doc = load_delete_next(Conn, ?HASH_DOWNLOAD_COLL),
	check_in_index_cache(Conn, Doc).

check_in_index_cache(_, {}) ->
	timer:send_after(?WAIT_TIME, timeout),
	empty;
check_in_index_cache(Conn, {Doc}) ->
	{Hash} = bson:lookup(hash, Doc),
	ListHash = binary_to_list(Hash),
	Try = should_try_download(config:get(check_cache, false), Conn, ListHash),
	case Try of
		true ->
			schedule_next({Doc}, true);
		false -> 
			% not in the local cache index, which means it may not exist on the server
			% so give it up
			hash_reader_stats:handle_cache_filtered(),
			self() ! filter_torrent
	end,
	ok.

should_try_download(true, Conn, Hash) ->
	db_hash_index:exist(Conn, Hash);
should_try_download(false, _, _) ->
	true.

% if there's no hash, try `wait_download' 
try_next(Conn) ->
	Doc = load_delete_next(Conn, ?HASH_COLLNAME),
	schedule_next(Doc, false).

load_delete_next(Conn, Col) ->
	Cmd = {findAndModify, Col, fields, {}, remove, true},
	Ret = mongo:do(safe, master, Conn, ?HASH_DBNAME, fun() ->
		mongo:command(Cmd)
	end),
	case Ret of
		{value, undefined, ok, 1.0} -> {};
		{value, Obj, lastErrorObject, _, ok, 1.0} -> {Obj}
	end.

schedule_next({}, true) ->
	ok;

schedule_next({}, false) ->
	?T("hash is empty, try to startup downloader"),
	self() ! process_download_hash;

schedule_next({Doc}, DownloadDoc) ->
	gen_server:cast(self(), {process_hash, Doc, DownloadDoc}).

db_conn(State) ->
	#state{dbpool = DBPool} = State,
	mongo_pool:get(DBPool).

