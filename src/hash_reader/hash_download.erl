%%
%% hash_download.erl
%% Kevin Lynx
%% 07.21.2013
%%
-module(hash_download).
-include("vlog.hrl").
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1]).
-record(state, {dbpool, downloader, downloading = 0, max}).
-define(WAIT_TIME, 1*60*1000).

start_link(DBPool) ->
	gen_server:start_link(?MODULE, [DBPool], []).

init([DBPool]) ->
	{ok, DownPid} = tor_download:start_link(),
	tor_download_stats:register(DownPid),
	Max = config:get(max_download_per_reader, 50),
	{ok, #state{dbpool = DBPool, downloader = DownPid, max = Max}, 0}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast({process_hash, Doc}, State) ->
	#state{downloading = DownCnt, downloader = DownPid, max = Max} = State,
	{BHash} = bson:lookup('_id', Doc),
	Hash = binary_to_list(BHash),
	ReqCnt = hash_reader_common:get_req_cnt(Doc),
	Conn = db_conn(State),
	AddDown = case db_store_mongo:inc_announce(Conn, Hash, ReqCnt) of
		true -> 
			?T(?FMT("hash ~s already exists in db", [Hash])),
			hash_reader_common:on_updated(Conn),
			0;
		false ->
			schedule_download(Conn, DownPid, Hash)
	end,
	case AddDown + DownCnt < Max of
		true ->
			schedule_next();
		false ->
			?T(?FMT("reached the max download ~p, wait", [Max])),
			wait_downloader_notify
	end,
	{noreply, State#state{downloading = DownCnt + AddDown}};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info({got_torrent, failed, _Hash}, State) ->
	#state{downloading = D} = State,
	schedule_next(),
	hash_reader_stats:handle_download_failed(),
	{noreply, State#state{downloading = D - 1}};

handle_info({got_torrent, ok, Hash, Content}, State) ->
	schedule_next(),
	Conn = db_conn(State),
	true = is_binary(Content),
	SaveTor = config:get(save_torrent, true),
	if SaveTor -> loc_torrent_cache:save(Conn, Hash, Content); true -> ok end,
	NewState = got_torrent_content(State, Hash, Content),
	hash_reader_stats:handle_download_ok(),
	{noreply, NewState};

handle_info({got_torrent_from_cache, Hash, Content}, State) ->
	on_used_cache(),
	schedule_next(),
	NewState = got_torrent_content(State, Hash, Content),
	{noreply, NewState};

handle_info(timeout, State) ->
	schedule_next(),
	{noreply, State}.

schedule_next() ->
	case hash_download_cache:get_one() of
		{} ->
			timer:send_after(?WAIT_TIME);
		Doc ->
			gen_server:cast(self(), {process_hash, Doc})
	end.

schedule_download(Conn, Pid, Hash) ->
	TryFilter = config:get(check_cache, false),
	Down = case TryFilter of
		true ->
			db_hash_index:exist(Conn, Hash);
		false ->
			true
	end,
	try_download(Down, Conn, Pid, Hash).

try_download(false, _, _, Hash) ->
	?T(?FMT("hash does not exist in index_cache, filter it ~s", [Hash])),
	0;
try_download(true, Conn, Pid, Hash) ->
	case loc_torrent_cache:load(Conn, Hash) of
		not_found -> 
			tor_download:download(Pid, Hash);
		Content -> 
			?T(?FMT("found torrent in local cache ~s", [Hash])),
			self() ! {got_torrent_from_cache, Hash, Content}
	end,
	1.

db_conn(State) ->
	#state{dbpool = DBPool} = State,
	mongo_pool:get(DBPool).

got_torrent_content(State, MagHash, Content) ->
	#state{downloading = D} = State,
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

