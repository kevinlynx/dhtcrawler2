%%
%% crawler_app.erl
%% Kevin Lynx
%% 06.19.2013
%%
-module(crawler_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([start/0, startboth/0, stop/0]).

start(_Type, _StartArgs) ->
	File = config_file_name(),
	io:format("load config file ~s ", [File]),
	case file:consult(File) of
		{error, _Reason} ->
			do_default_start(File);
		{ok, [Cfg]} ->
			do_start(Cfg)
	end.

stop(_State) ->
	crawler_sup:stop().

config_file_name() ->
	filename:join([filename:dirname(code:which(?MODULE)), 
		"..", "priv", "dhtcrawler.config"]).

do_default_start(File) ->
	List = [{start_port, 6776},
		{node_count, 10},
		{loglevel, 3},
		{dbconn, 15},
		{dbhost, "localhost"},
		{dbport, 27017}],
	filelib:ensure_dir("priv/"),
	file:write_file(File, io_lib:fwrite("~p.\n",[List])),
	do_start(List).

do_start(List) ->
	StartPort = proplists:get_value(start_port, List),
	Count = proplists:get_value(node_count, List),
	LogLevel = proplists:get_value(loglevel, List),
	DBConn = proplists:get_value(dbconn, List),
	DBHost = proplists:get_value(dbhost, List),
	DBPort = proplists:get_value(dbport, List),
	io:format("dhtcrawler startup ~p, ~p, ~p:~p~n", [StartPort, Count, DBHost, DBPort]),
	crawler_sup:start_link({StartPort, Count, DBHost, DBPort, LogLevel, DBConn}).

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

startboth() ->
    start(),
    crawler_http:start().

