%%
%% hash_reader_stats.erl
%% Kevin Lynx
%% 06.29.2013
%%
-module(hash_reader_stats).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/2,
		 start_link/1,
		 stop/0,
		 handle_update/0,
		 handle_insert/0,
		 handle_used_cache/0,
		 dump/0]).
-record(state, {tref, count, start, name, cache_used = 0, updated = 0, inserted = 0}).
-define(STATS_INTERVAL, 10*60*1000).
-define(TEXT(Fmt, Arg), lists:flatten(io_lib:format(Fmt, Arg))).

start_link(Count) ->
	start_link(Count, "reader_stats.txt").

start_link(Count, Name) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [Count, Name], []).

stop() ->
	gen_server:cast(srv_name(), stop).

dump() ->
	gen_server:cast(srv_name(), dump).

handle_update() ->
	gen_server:cast(srv_name(), inc_updated).

handle_insert() ->
	gen_server:cast(srv_name(), inc_inserted).

handle_used_cache() ->
	gen_server:cast(srv_name(), inc_cache_used).

srv_name() ->
	?MODULE.

init([Count, Name]) ->
	{ok, TRef} = timer:send_interval(?STATS_INTERVAL, dump),
	State = #state{tref = TRef, count = Count, name = Name, start = now()},
	{ok, State}.

terminate(_, State) ->
	#state{tref = TRef} = State,
	timer:cancel(TRef),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(dump, State) ->
	do_dump(State),
	{noreply, State};

handle_info(_, State) ->
	{noreply, State}.

handle_cast(dump, State) ->
	do_dump(State),
	{noreply, State};

handle_cast(inc_updated, State) ->
	#state{updated = U} = State,
	{noreply, State#state{updated = U + 1}};

handle_cast(inc_inserted, State) ->
	#state{inserted = I} = State,
	{noreply, State#state{inserted = I + 1}};

handle_cast(inc_cache_used, State) ->
	#state{cache_used = C} = State,
	{noreply, State#state{cache_used = C + 1}};
	
handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

do_dump(State) ->
	Dir = "log/",
	#state{name = Name} = State,
	filelib:ensure_dir(Dir),
	% this will cause a long time waiting
	DownloadStats = format_download_stats(),
	{ok, FP} = file:open(Dir ++ Name, [append]),
	io:format(FP, "~s~n", [date_string()]),
	io:format(FP, "~p~n", [self()]),
	io:format(FP, "~s~n", [format_stats(State)]),
	io:format(FP, "~s~n", [DownloadStats]),
	file:close(FP).

stats_time(Start) ->
	DiffSecs = timer:now_diff(now(), Start) div 1000 div 1000,
	% {day, {hour, min, sec}}
	calendar:seconds_to_daystime(DiffSecs).

date_string() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    lists:flatten(io_lib:format("~b-~2.10.0b-~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b", 
    	[Year, Month, Day, Hour, Min, Sec])).

format_stats(State) ->
	#state{count = C, start = Start, cache_used = Cache, updated = U, inserted = I} = State,
	{Day, {H, M, S}} = stats_time(Start),
	Mins = Day * 24 * 60 + H * 60 + M,
	TotalMins = if Mins > 0 -> Mins; true -> 1 end,
	Speed = (U + I) div TotalMins,
	InsertPercent = I * 100 / (if U > 0 -> U; true -> 1 end),
	CachePercent = Cache * 100 / (if I > 0 -> I; true -> 1 end),
	?TEXT("  stats time ~b ~2.10.0b:~2.10.0b:~2.10.0b~n", 
		[Day, H, M, S]) ++
	?TEXT("  Reader count ~p~n", [C]) ++
	?TEXT("  Process speed ~p req/min~n", [Speed]) ++
	?TEXT("  Download torrents speed ~p tor/min~n", [I div TotalMins]) ++
	?TEXT("  Updated ~p~n", [U]) ++
	?TEXT("  Inserted ~p~n", [I]) ++
	?TEXT("  Inserted percentage ~.2f%~n", [InsertPercent]) ++
	?TEXT("  Used cache torrents ~p (~.2f%)~n", [Cache, CachePercent]).

format_download_stats() ->
	Start = now(),	
	{ProcessCount, HashSum, ReqSum, TotalTime, CurrentReqCount} = 
		tor_download_stats:stats(),
	TotalSecs = TotalTime div 1000,
	TorSpeed = if HashSum > 0 -> TotalSecs div HashSum; true -> 0 end,
	Used = timer:now_diff(now(), Start) div 1000,
	?TEXT("  ==== Torrent download stats ====~n", []) ++
	?TEXT("  Stats used time ~p ms~n", [Used]) ++
	?TEXT("  Downloader count ~p~n", [ProcessCount]) ++
	?TEXT("  Request torrents ~p~n", [HashSum]) ++
	?TEXT("  Http requests ~p~n", [ReqSum]) ++
	?TEXT("  Total used time ~p secs~n", [TotalSecs]) ++
	?TEXT("  Download speed ~p tor/secs~n", [TorSpeed]) ++
	?TEXT("  Current wait requests ~p~n", [CurrentReqCount]).



