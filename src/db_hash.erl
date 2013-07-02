%%
%% db_hash.erl
%% Kevin Lynx
%% 06.28.2013
%% save info_hash to database
%%
-module(db_hash).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/2, 
	     start_link/4,
	     insert/1,
	     stop/0]).
-record(state, {hash_docs, max_cache, index}).
-define(POOL_NAME, hash_db_pool).
-include("db_common.hrl").

start_link(IP, Port, Size, MaxCache) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [IP, Port, Size, MaxCache], []).

start_link(IP, Port) ->
	start_link(IP, Port, 5, 10).

stop() ->
	gen_server:cast(srv_name(), stop).

% magnet hash, binary string
insert(Hash) when is_list(Hash) ->
	gen_server:cast(srv_name(), {insert, list_to_binary(Hash)}).

srv_name() ->
	db_hash.

init([IP, Port, Size, MaxCache]) ->
	mongo_sup:start_pool(?POOL_NAME, Size, {IP, Port}),
	process_flag(trap_exit, true),
	{_, Index} = db_system:load_batch_index(mongo_pool:get(?POOL_NAME)),
	{ok, #state{hash_docs = [], max_cache = MaxCache, index = Index}}.

terminate(_, State) ->
	mongo_sup:stop_pool(?POOL_NAME),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast({insert, Hash}, State) ->
	#state{hash_docs = List, max_cache = MaxCache, index = Index} = State,
	{NewIndex, NewList} = try_insert(Index, Hash, List, MaxCache),
	{noreply, State#state{hash_docs = NewList, index = NewIndex}};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

%% DB stuff
try_insert(Index, Hash, List, MaxCache) when length(List) + 1 == MaxCache ->
	Conn = mongo_pool:get(?POOL_NAME),
	mongo:do(unsafe, master, Conn, ?HASH_DBNAME, fun() ->
		mongo:insert(?HASH_COLLNAME, [create_hash_doc(Index, Hash)|List])
	end),
	db_system:inc_batch_windex(Conn),
	db_system:stats_query_inserted(Conn, MaxCache),
	{Index + 1, []};

try_insert(Index, Hash, List, _) ->
	Doc = create_hash_doc(Index, Hash),
	{Index, [Doc|List]}.

create_hash_doc(Index, Hash) when is_binary(Hash) ->
	{hash, Hash, index, Index, req_at, time_util:now_seconds()}.


