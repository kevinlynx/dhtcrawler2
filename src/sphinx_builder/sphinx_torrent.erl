%%
%% sphinx_torrent.erl
%% Kevin Lynx
%% 07.29.2013
%%
-module(sphinx_torrent).
-include("vlog.hrl").
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/3, get/0, try_times/0]).
-export([do_load_torrents/2]). % disable warning only
-define(DBNAME, torrents).
-define(COLLNAME, hashes).
-define(POOLNAME, db_pool).
-define(WAIT_TIME, 30*1000).
-record(state, {offset = 0, max, try_times = 0, tors = [], cursor}).

start_link(IP, Port, Offset) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [IP, Port, Offset], []).

get() ->
	gen_server:call(srv_name(), get, infinity).

try_times() ->
	gen_server:call(srv_name(), try_times).

%%
srv_name() ->
	sphinx_torrent_loader.

init([IP, Port, Offset]) ->
	Max = config:get(torrent_batch_count, 100),
	mongo_sup:start_pool(?POOLNAME, 5, {IP, Port}),
	{ok, #state{offset = Offset, max = Max}, 0}.

handle_cast(load, State) ->
	#state{cursor = Cursor, offset = Skip, max = Max, tors = Tors} = State,
	Request = Max * 2 div 3,
	?T(?FMT("request next ~p torrents", [Request])),
	LoadTors = load_next_batch(Cursor, Request),
	case length(LoadTors) of
		0 -> 
			?T(?FMT("no torrents in cursor ~p", [Cursor])),
			mongo_cursor:close(Cursor),
			timer:send_after(?WAIT_TIME, try_load);
		_ -> ok
	end,
	?T(?FMT("load ~p torrents", [length(LoadTors)])),
	NewOffset = Skip + length(LoadTors),
	{noreply, State#state{offset = NewOffset, tors = Tors ++ LoadTors}};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(try_load, State) ->
	#state{offset = Skip, max = Max, try_times = Try} = State,
	?T(?FMT("try load ~p torrents from ~p", [Max, Skip])),
	case load_cursor_batch(Skip, Max) of
		{} -> 
			timer:send_after(?WAIT_TIME, try_load),
			{noreply, State#state{try_times = Try + 1}};
		{Cursor, R} ->
			{noreply, State#state{try_times = 0, offset = Skip + length(R), tors = R, cursor = Cursor}}
	end;

handle_info(timeout, State) ->
	self() ! try_load,
	{noreply, State}.

handle_call(try_times, _From, #state{try_times = Try} = State) ->
	{reply, Try, State};

handle_call(get, _From, State) ->
	#state{tors = Tors, max = Max} = State,
	{Tor, NewTors} = case length(Tors) of
		0 -> {{}, []};
		_ -> [H|Rest] = Tors, {H, Rest}
	end,
	try_load_next(NewTors, Max),
	{reply, Tor, State#state{tors = NewTors}}.

terminate(_, State) ->
	mongo_sup:stop_pool(?POOLNAME),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

try_load_next([], _) ->
	?T("no torrents"),
	skip;
try_load_next(Tors, Max) when length(Tors) == Max div 3 ->
	?T(?FMT("attempt to load next ~p torrents", [Max * 2 div 3])),
	gen_server:cast(self(), load);	
try_load_next(_, _) ->
	ok.

load_cursor(Skip, Size) ->
	Conn = mongo_pool:get(?POOLNAME),
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:find(?COLLNAME, {}, {}, Skip, Size)
	end).

load_cursor_batch(Skip, Size) ->
	Cursor = load_cursor(Skip, Size),
	case load_next_batch(Cursor, Size) of
		[] ->
			mongo_cursor:close(Cursor), {};
		R ->
			{Cursor, R}
	end.

% will cause `get_more'
load_next_batch(Cursor, Size) ->
	mongo_cursor:take(Cursor, Size).

% will cause lots of queries
do_load_torrents(Skip, Size) ->
	Conn = mongo_pool:get(?POOLNAME),
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		% 1: cost lots of memory even close the cursor
		%Cursor = mongo:find(?COLLNAME, {}, {}, Skip, Size),
		%Ret = mongo_cursor:rest(Cursor), but close the cursor will reduce the memory
		% 2: cost lots of memory more than solution 1
		%Cursor = mongo:find(?COLLNAME, {}, {}, Skip),
		%mongo_cursor:take(Cursor, Size),
		% 3:
		Cursor = mongo:find(?COLLNAME, {}, {}, Skip),
		Ret = mongo_cursor:take(Cursor, Size),
		mongo_cursor:close(Cursor),
		Ret
	end).

