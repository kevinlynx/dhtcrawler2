%%
%% crawler_app.erl
%% Kevin Lynx
%% 06.19.2013
%%
-module(crawler_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([start/0, stop/0]).

% application behaviour callback
start(_Type, _StartArgs) ->
	config:start_link("dhtcrawler.config", fun() -> config_default() end),
	do_start().

stop(_State) ->
	config:stop(),
	crawler_sup:stop().

do_start() ->
	StartPort = config:get(start_port),
	Count = config:get(node_count),
	LogLevel = config:get(loglevel),
	DBConn = config:get(dbconn),
	DBHost = config:get(dbhost),
	DBPort = config:get(dbport),
	CacheMaxCount = config:get(hash_max_cache),
	CacheMaxTime = config:get(cache_max_time),
	io:format("dhtcrawler startup ~p, ~p, ~p:~p~n", [StartPort, Count, DBHost, DBPort]),
	crawler_sup:start_link({StartPort, Count, DBHost, DBPort, LogLevel, DBConn, CacheMaxTime, CacheMaxCount}).

start() ->
	error_logger:logfile({open, "crash.log"}),
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/kdht/ebin"),
	code:add_path("deps/mongodb/ebin"),
	Apps = [asn1, crypto, public_key, ssl, inets, bson, mongodb],	
	[application:start(App) || App <- Apps],
	application:start(dhtcrawler).

stop() ->
	application:stop(dhtcrawler).

config_default() ->
	[{start_port, 6776},
	 {node_count, 50},
	 {hash_max_cache, 300},
	 {cache_max_time, 2*60}, % seconds
	 {loglevel, 3},
	 {dbconn, 5},
	 {dbhost, "localhost"},
	 {dbport, 27017}].

