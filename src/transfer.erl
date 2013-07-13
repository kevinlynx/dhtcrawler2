-module(transfer).
-compile(export_all).
-export([start/0]).
-define(DBNAME, torrents).
-define(COLLNAME, hash).
-define(ONE_DAY, 24*60*60).
-define(READ_POOL, read_pool).
-define(WRITE_POOL, write_pool).
-export([start/2]).

start() ->
	mongo_sup:start_pool(?READ_POOL, 5, {localhost, 10000}),
	mongo_sup:start_pool(?WRITE_POOL, 5, {localhost, 27010}),
	[spawn(?MODULE, start, [DayStart, DayEnd]) ||
		{DayStart, DayEnd} <- gen_day_ranges()],
	ok.

gen_day_ranges() ->
	Today = time_util:now_day_seconds(),
	[day_secs_at(Today, Before) || Before <- lists:seq(0, 15)].

day_secs_at(Today, Before) ->
	{Today - Before * ?ONE_DAY, Today - Before * ?ONE_DAY + ?ONE_DAY}.

start(DaySecs, DaySecsMax) ->
	RConn = mongo_pool:get(?READ_POOL),
	WConn = mongo_pool:get(?WRITE_POOL),
	Docs = mongo:do(safe, master, RConn, ?DBNAME, fun() ->
		Cursor = mongo:find(?COLLNAME, {created_at, {'$gt', DaySecs, '$lt', DaySecsMax}}),
		mongo_cursor:rest(Cursor)
	end),
	case Docs of 
		[] ->
			ok;
		_ ->	
			mongo:do(safe, master, WConn, ?DBNAME, fun() ->
				mongo:insert(?COLLNAME, Docs)
			end)
	end,
	io:format("done at ~p size ~p~n", [DaySecs, length(Docs)]).



