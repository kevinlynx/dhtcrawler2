%%
%% hash_download_cache.erl
%% Kevin Lynx
%% cache these wait_download hashes, the downloader will read hashes from here,
%% to avoid database operation, if the cache is too big, save it then.
%% 07.21.2013
%% 
-module(hash_download_cache).
-include("vlog.hrl").
-include("db_common.hrl").
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1,
		 stop/0,
		 insert/1,
		 get_one/0]).
-record(state, {cache = [], max, dbpool}).
-define(SAVE_BATCH, 100).

start_link(DBPool) ->
	Max = config:get(max_download_cache, 100),
	gen_server:start_link({local, srv_name()}, ?MODULE, [DBPool, Max], []).

stop() ->
	gen_server:cast(srv_name(), stop).

insert(Doc) ->
	gen_server:cast(srv_name(), {insert, Doc}).

get_one() ->
	gen_server:call(srv_name(), get_one, infinity).

srv_name() ->
	?MODULE.

%
init([DBPool, Max]) ->
	{ok, #state{max = Max, dbpool = DBPool}}.

terminate(_, State) ->
	#state{dbpool = DBPool, cache = Cache} = State,
	check_save(DBPool, Cache, 0),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast({insert, Doc}, State) ->
	#state{dbpool = DBPool, cache = Cache, max = Max} = State,
	NewCache = check_save(DBPool, [Doc|Cache], Max),
	{noreply, State#state{cache = NewCache}};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(get_one, _From, State) ->
	#state{dbpool = DBPool, cache = Cache} = State,
	{Doc, NewCache} = try_load(DBPool, Cache),
	{reply, Doc, State#state{cache = NewCache}};

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

check_save(DBPool, Cache, Max) when length(Cache) >= Max ->
	SplitCnt = 2 * Max div 3,
	{Remain, ToSave} = lists:split(SplitCnt, Cache),
	?T(?FMT("download_cache reached the max, save 1/3 ~p", [length(ToSave)])),
	do_save(DBPool, ToSave),
	Remain;
check_save(_, Cache, _) ->
	Cache.

do_save(_DBPool, []) ->
	ok;
do_save(DBPool, Docs) ->
	Insert = fun(Doc) ->
		Conn = mongo_pool:get(DBPool),
		{ID} = bson:lookup('_id', Doc),
		Sel = {'_id', ID},
		mongo:do(safe, master, Conn, ?HASH_DBNAME, fun() ->
			mongo:update(?HASH_DOWNLOAD_COLL, Sel, Doc, true)
		end)
	end,
	[Insert(Doc) || Doc <- Docs].

try_load(DBPool, []) ->
	?T("download_cache empty, load hash from db"),
	Conn = mongo_pool:get(DBPool),
	{Doc} = hash_reader_common:load_delete_doc(Conn, ?HASH_DOWNLOAD_COLL),
	{Doc, []};
try_load(_, [First|Rest]) ->
	{First, Rest}.

