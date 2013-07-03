%%
%% db_hash_reader.erl
%% Kevin Lynx
%% 06.28.2013
%%
-module(db_hash_reader).
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

handle_info({got_torrent, failed, _Hash}, State) ->
	#state{downloading = D} = State,
	Conn = db_conn(State),
	try_next_download(Conn),
	{noreply, State#state{downloading = D - 1}};

handle_info({got_torrent, ok, Hash, Content}, State) ->
	#state{downloading = D} = State,
	Conn = db_conn(State),
	try_next_download(Conn),
	case catch(torrent_file:parse(Content)) of
		{'EXIT', _} ->
			State;
		{Type, Info} -> 
			got_torrent(State, Hash, Type, Info)
	end,
	{noreply, State#state{downloading = D - 1}};

handle_info(timeout, State) ->
	Conn = db_conn(State),
	try_next(Conn),
	{noreply, State};

handle_info(_, State) ->
	{noreply, State}.

handle_cast({process_hash, Doc,  DownloadDoc}, State) ->
	Conn = db_conn(State),
	{Hash} = bson:lookup(hash, Doc),
	ListHash = binary_to_list(Hash),
	% to avoid register many timers when the hash is empty but download hash is not
	% it also can avoid increase the message queue size, everytime this function get called,
	% it remove this message and append only another 1.
	if DownloadDoc -> do_nothing; true -> try_next(Conn) end,
	NewState = case db_store_mongo:inc_announce(Conn, ListHash) of
		true -> 
			?T(?FMT("inc_announce success ~s", [ListHash])),
			on_updated(Conn),
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

try_download(State, Hash, Doc) ->
	#state{downloading = D} = State,
	Conn = db_conn(State),
	NewDownloading = case D >= ?MAX_DOWNLOAD of
		true -> % put it into the download queue
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
	case db_loc_torrent:load(Conn, Hash) of
		not_found -> % not in the local cache, download it now
			tor_download:download(Pid, Hash);
		Content -> % process later
			self() ! {got_torrent, ok, Hash, Content}
	end.

try_save(State, Hash, Name, Length, Files) ->
	Conn = db_conn(State),
	case catch db_store_mongo:insert(Conn, Hash, Name, Length, Files) of
		{'EXIT', Reason} ->
			?E(?FMT("save torrent failed ~p", [Reason]));
		_ -> 
			on_saved(Conn)
	end.

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
	Doc = mongo:do(safe, master, Conn, ?HASH_DBNAME, fun() ->
		D = mongo:find_one(?HASH_DOWNLOAD_COLL, {}),
		delete_inner_doc(?HASH_DOWNLOAD_COLL, D),
		D
	end),
	schedule_next(Doc, true).

try_next(Conn) ->
	Doc = mongo:do(safe, master, Conn, ?HASH_DBNAME, fun() ->
		D = mongo:find_one(?HASH_COLLNAME, {}),
		delete_inner_doc(?HASH_COLLNAME, D),
		D
	end),
	schedule_next(Doc, false).

delete_inner_doc(_Col, {}) ->
	ok;

delete_inner_doc(Col, {Doc}) ->
	{ID} = bson:lookup('_id', Doc),
	mongo:delete(Col, {'_id', ID}).

schedule_next({}, true) ->
	ok;

schedule_next({}, false) ->
	timer:send_after(?WAIT_TIME, timeout);

schedule_next({Doc}, DownloadDoc) ->
	gen_server:cast(self(), {process_hash, Doc, DownloadDoc}).

db_conn(State) ->
	#state{dbpool = DBPool} = State,
	mongo_pool:get(DBPool).

