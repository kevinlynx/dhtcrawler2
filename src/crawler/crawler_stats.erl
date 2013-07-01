%%
%% crawler_stats.erl
%% Kevin Lynx
%% 06.17.2013
%%
-module(crawler_stats).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/0,
		 stop/0,
		 saved/1,
		 announce/0,
		 get_peers/0,
		 dump/0,
		 get_stats_desc/0,
		 get_stats/0]).
-export([handle_new/0, 
		 handle_update/0,
		 handle_init/1]).
-include("crawler_stats.hrl").
-define(DUMP_INTERVAL, 30*60*1000).

start_link() ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

stop() ->
    gen_server:cast(srv_name(), stop).

announce() ->
	gen_server:cast(srv_name(), {announce}).

get_peers() ->
	gen_server:cast(srv_name(), {get_peers}).

saved(New) ->
	Type = case New of
		true -> new_torrent;
		false -> update_torrent
	end,	
	gen_server:cast(srv_name(), {Type}).

get_stats() ->
	gen_server:call(srv_name(), {get_stats}).

get_stats_desc() ->
	gen_server:call(srv_name(), {get_stats_desc}).

% TODO: re-arrange the process relationship
handle_init(Sum) ->
	start_link(),
	gen_server:call(srv_name(), {set_sum, Sum}).

handle_new() ->
	saved(true).

handle_update() ->
	saved(false).	

dump() ->
	gen_server:cast(srv_name(), {dump}).

srv_name() ->
    ?MODULE.

init([]) ->
	timer:send_interval(?DUMP_INTERVAL, {dump_stats}), % cancel the timer ?
	{ok, #crawler_stats{start_time = now(), torrent_sum = 0}}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast({new_torrent}, State) ->
	#crawler_stats{new_saved = Saved, torrent_sum = Sum, torrent_count = ThisCount} = State,
	{noreply, State#crawler_stats{new_saved = Saved + 1, 
		torrent_count = ThisCount + 1,
		torrent_sum = Sum + 1}};

handle_cast({update_torrent}, State) ->
	#crawler_stats{updated = U, torrent_count = ThisCount} = State,
	{noreply, State#crawler_stats{updated = U + 1, torrent_count = ThisCount + 1}};

handle_cast({announce}, State) ->
	#crawler_stats{announce_count = A} = State,
	{noreply, State#crawler_stats{announce_count = A + 1}};

handle_cast({get_peers}, State) ->
	#crawler_stats{get_peers_count = G} = State,
	{noreply, State#crawler_stats{get_peers_count = G + 1}};

handle_cast({dump}, State) ->
	do_dump(State),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({dump_stats}, State) ->
	do_dump(State),
	{noreply, State};

handle_info(_, State) ->
	{noreply, State}.

handle_call({set_sum, Sum}, _From, State) ->
	{reply, ok, State#crawler_stats{torrent_sum = Sum}};

handle_call({get_stats_desc}, _From, State) ->
	{reply, format_stats(State), State};

handle_call({get_stats}, _From, State) ->
	Elapsed = stats_time(State#crawler_stats.start_time),
	Ret = {Elapsed, State},
	{reply, Ret, State};

handle_call(_, _From, State) ->
	{noreply, State}.

stats_time(Start) ->
	DiffSecs = timer:now_diff(now(), Start) div 1000 div 1000,
	% {day, {hour, min, sec}}
	calendar:seconds_to_daystime(DiffSecs).

date_string() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    lists:flatten(io_lib:format("~b-~2.10.0b-~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b", 
    	[Year, Month, Day, Hour, Min, Sec])).

-define(TEXT(Fmt, Arg), lists:flatten(io_lib:format(Fmt, Arg))).

do_dump(State) ->
	{ok, FP} = file:open("dhtcrawler-stats.txt", [append]),
	io:format(FP, "~s~n", [date_string()]),
	io:format(FP, "~s~n", [format_stats(State)]),
	file:close(FP).

format_stats(State) ->
	#crawler_stats{
		get_peers_count = G,
		announce_count = A,
		torrent_count = ThisCount,
		new_saved = New,
		updated = U,
		torrent_sum = Sum
	} = State,
	{Day, {H, M, S}} = stats_time(State#crawler_stats.start_time),
	?TEXT("  stats time ~b ~2.10.0b:~2.10.0b:~2.10.0b~n", 
		[Day, H, M, S]) ++
	?TEXT("  torrent sum      ~b~n", [Sum]) ++
	?TEXT("  get_peers count  ~b~n", [G]) ++
	?TEXT("  announce count   ~b~n", [A]) ++
	?TEXT("  download torrent ~b~n", [ThisCount]) ++
	?TEXT("  new saved        ~b~n", [New]) ++
	?TEXT("  updated          ~b~n", [U]).
