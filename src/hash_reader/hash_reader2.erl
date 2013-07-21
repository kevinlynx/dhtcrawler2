%%
%% hash_reader.erl
%% Kevin Lynx
%% 07.21.2013
%%
-module(hash_reader2).
-include("vlog.hrl").
-include("db_common.hrl").
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1]).
-define(WAIT_TIME, 1*60*1000).
-record(state, {dbpool}).

start_link(DBPool) ->
	gen_server:start_link(?MODULE, [DBPool], []).

init([DBPool]) ->
	hash_download:start_link(DBPool),
	{ok, #state{dbpool = DBPool}, 0}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast({process_hash, Doc}, State) ->
	{BHash} = bson:lookup('_id', Doc),
	Hash = binary_to_list(BHash),
	ReqCnt = hash_reader_common:get_req_cnt(Doc),
	Conn = db_conn(State),
	case db_store_mongo:inc_announce(Conn, Hash, ReqCnt) of
		true -> 
			hash_reader_common:on_updated(Conn);
		false ->
			?T(?FMT("insert doc ~s to download_cache", [Hash])),
			hash_download_cache:insert(Doc)
	end,
	schedule_next(Conn),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info(timeout, State) ->
	Conn = db_conn(State),
	schedule_next(Conn),
	{noreply, State}.

schedule_next(Conn) ->
	case hash_reader_common:load_delete_doc(Conn, ?HASH_COLLNAME) of
		{} ->
			?T("start to wait for new hash"),
			timer:send_after(?WAIT_TIME, timeout);
		{Doc} ->
			gen_server:cast(self(), {process_hash, Doc})
	end.

db_conn(State) ->
	#state{dbpool = DBPool} = State,
	mongo_pool:get(DBPool).

