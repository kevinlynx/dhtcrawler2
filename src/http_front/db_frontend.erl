%%
%% db_frontend.erl
%% Kevin Lynx
%% 07.02.2013
%%
-module(db_frontend).
-export([start/3,
		 search/1,
		 search/3,
		 today_top/0,
		 search_one/1,
		 stats/0,
		 all_top/0,
		 newest/0,
	  	 stop/0]).
-define(DB_POOLNAME, db_pool_frontend).
-define(ONEDAY_SECS, 24*60*60).

start(IP, Port, PoolSize) ->
	mongo_sup:start_pool(?DB_POOLNAME, PoolSize, {IP, Port}),
	Conn = mongo_pool:get(?DB_POOLNAME),
	db_store_mongo:init(Conn).

stop() ->
	mongo_sup:stop_pool(?DB_POOLNAME).

search(Keyword) ->
	search(Keyword, 0, 0).

search(Keyword, Page, Count) ->
	Conn = mongo_pool:get(?DB_POOLNAME),
	case config:get(search_method, mongodb) of
		mongodb ->
			db_store_mongo:search(Conn, Keyword);
		sphinx ->
			Offset = Page * Count,
			sphinx_search:search(Conn, Keyword, Offset, Count)
	end.

today_top() ->
	Conn = mongo_pool:get(?DB_POOLNAME),
	DaySecs = time_util:now_day_seconds(),
	find_day_top(Conn, DaySecs, 10).

search_one(MagHash) ->
	Conn = mongo_pool:get(?DB_POOLNAME),
	db_store_mongo:index(Conn, MagHash).

find_day_top(_Conn, _DaySecs, 0) ->
	[];
find_day_top(Conn, DaySecs, Try) ->
	case db_store_mongo:search_newest_top_by_date(Conn, 50, DaySecs) of
		[] ->
			find_day_top(Conn, DaySecs - ?ONEDAY_SECS, Try - 1);
		List ->
			List
	end.

% {TorSum, [{DaySec, Processed, RecvQuery, Updated, New}, xx]}
stats() ->
	Conn = mongo_pool:get(?DB_POOLNAME),
	DaySecs = time_util:now_day_seconds(),
	TorSum = db_store_mongo:count(Conn),
	D1 = db_system:stats_day_at_slave(Conn, DaySecs),
	D2 = db_system:stats_day_at_slave(Conn, DaySecs - ?ONEDAY_SECS),
	D3 = db_system:stats_day_at_slave(Conn, DaySecs - 2 * ?ONEDAY_SECS),
	{TorSum, [D1, D2, D3]}.

% test only
all_top() ->
	Conn = mongo_pool:get(?DB_POOLNAME),
	db_store_mongo:search_announce_top(Conn, 50).

newest() ->
	Conn = mongo_pool:get(?DB_POOLNAME),
	db_store_mongo:search_recently(Conn, 50).

	
