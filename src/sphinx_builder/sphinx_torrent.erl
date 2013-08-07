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
-export([start_link/3, get/0, offset_date/0, try_times/0]).
-import(time_util, [date_time_string/1]).
-define(DBNAME, torrents).
-define(COLLNAME, hashes).
-define(POOLNAME, db_pool).
-define(WAIT_TIME, 30*1000).
-define(DATE_RANGE, 12*60*60).
-record(state, {max, try_times = 0, tors = [], date}).

start_link(IP, Port, Date) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [IP, Port, Date], []).

get() ->
	gen_server:call(srv_name(), get, infinity).

try_times() ->
	gen_server:call(srv_name(), try_times, infinity).

offset_date() ->
	gen_server:call(srv_name(), date, infinity).

%%
srv_name() ->
	sphinx_torrent_loader.

init([IP, Port, Date]) ->
	Max = config:get(torrent_batch_count, 100),
	mongo_sup:start_pool(?POOLNAME, 5, {IP, Port}),
	Conn = mongo_pool:get(?POOLNAME),
	db_store_mongo:ensure_date_index(Conn),
	CheckDate = find_first_date(Date),
	io:format("load torrent from ~s~n", [date_time_string(CheckDate)]),
	{ok, #state{date = CheckDate, max = Max}, 0}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(try_load, State) ->
	#state{date = Date, tors = Tors, max = Max, try_times = Try} = State,
	case length(Tors) < Max of 
		true ->
			?T(?FMT("try load more torrents ~p -> ~p", [length(Tors), Max])),
			case do_load_torrents(Date, Max) of
				[] ->
					?T(?FMT("load 0 torrent from ~s", [date_time_string(Date)])),
					NewDate = forward_date(Date),
					NewTry = case NewDate == Date of
						true ->
							?T(?FMT("no more torrents, wait, try ~p", [Try])),
							timer:send_after(?WAIT_TIME, try_load),
							Try + 1; 
						false -> 
							?T(?FMT("forward date to ~s", [date_time_string(NewDate)])),
							timer:send_after(100, try_load),
							Try 
					end,
					{noreply, State#state{date = NewDate, try_times = NewTry}};
				Ret ->
					timer:send_after(100, try_load),
					NewDate = query_created_at(lists:last(Ret)),
					?T(?FMT("load ~p torrents, new date ~s", [length(Ret), date_time_string(NewDate)])),
					{noreply, State#state{date = NewDate, tors = Tors ++ Ret, try_times = 0}}
			end;
		false ->
			timer:send_after(100, try_load),
			{noreply, State}
	end;

handle_info(timeout, State) ->
	self() ! try_load,
	{noreply, State}.

handle_call(try_times, _From, #state{try_times = Try} = State) ->
	{reply, Try, State};

handle_call(date, _From, #state{date = Date} = State) ->
	{reply, Date, State};

handle_call(get, _From, State) ->
	#state{tors = Tors} = State,
	{Tor, NewTors} = case length(Tors) of
		0 -> {{}, []};
		_ -> [H|Rest] = Tors, {H, Rest}
	end,
	{reply, Tor, State#state{tors = NewTors}}.

terminate(_, State) ->
	mongo_sup:stop_pool(?POOLNAME),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

forward_date(Date) ->
	NowSecs = time_util:now_seconds(),
	EndDate = Date + ?DATE_RANGE,
	if EndDate < NowSecs -> EndDate; true -> Date end.

% will cause lots of queries
do_load_torrents(Date, Size) ->
	Q = {created_at, {'$gt', Date, '$lte', Date + ?DATE_RANGE}},
	Conn = mongo_pool:get(?POOLNAME),
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		% 1: cost lots of memory even close the cursor
		%Cursor = mongo:find(?COLLNAME, {}, {}, Skip, Size),
		%Ret = mongo_cursor:rest(Cursor), but close the cursor will reduce the memory
		% 2: cost lots of memory more than solution 1
		%Cursor = mongo:find(?COLLNAME, {}, {}, Skip),
		%mongo_cursor:take(Cursor, Size),
		% 3:
		Cursor = mongo:find(?COLLNAME, Q, {}, 0, Size),
		Ret = mongo_cursor:take(Cursor, Size),
		mongo_cursor:close(Cursor),
		Ret
	end).

find_first_date(0) ->
	Conn = mongo_pool:get(?POOLNAME),
	Ret = mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:find_one(?COLLNAME, {})
	end),
	case Ret of
		{} -> 
			time_util:now_seconds();
		{Doc} ->
			query_created_at(Doc) - 1
	end;
find_first_date(Date) ->
	Date.

query_created_at(Tor) ->
	{CreatedT} = bson:lookup(created_at, Tor),
	CreatedT.

