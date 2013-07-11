%%
%% http_cache.erl
%% Kevin Lynx
%% 07.03.2013
%%
-module(http_cache).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/0, 
		 stop/0,
		 search/1,
		 stats/0,
		 today_top/0]).
-export([async_update/2]).
-record(state, {cache}).
-define(OUT_OF_DATE, 10*60*1000).
-define(CACHE_SIZE, 1000).

start_link() ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

stop() ->
	gen_server:cast(srv_name(), stop).

search(Key) ->
	gen_server:call(srv_name(), {query, {search, Key}}).

today_top() ->
	gen_server:call(srv_name(), {query, top}).

stats() ->
	gen_server:call(srv_name(), {query, stats}).

init([]) ->
	{ok, #state{cache = gb_trees:empty()}, 0}.

srv_name() ->
	http_cache.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast(decrease_cache, State) ->
	#state{cache = Cache} = State,
	NewCache = remove_oldest(Cache),
	spawn_update(top), % make sure `top' exists
	spawn_update(stats),
	{noreply, State#state{cache = NewCache}};

handle_cast({update, Type}, State) ->
	{NewState, _} = update(Type, State),
	{noreply, NewState};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call({query, Type}, _From, State) ->
	{NewState, Ret} = query(Type, State),
	{reply, Ret, NewState};

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info(timeout, State) ->
	spawn_update(top),
	spawn_update(stats),
	{noreply, State};

handle_info({enter_cache, Type, Time, Ret}, State) ->
	#state{cache = Cache} = State,
	Val = {Time, Ret},
	NewCache = gb_trees:enter(Type, Val, Cache),
	{noreply, State#state{cache = NewCache}};

handle_info(_, State) ->
	{noreply, State}.

% for debug purpose
query(top, State) ->
	{State, do_update(top)};

% for debug purpose
query(stats, State) ->
	{State, do_update(stats)};

query(Type, State) ->
	#state{cache = Cache} = State,
	case gb_trees:is_defined(Type, Cache) of
		false ->
			update(Type, State);
		true ->
			do_query(Type, State)
	end.

update(Type, #state{cache = Cache} = State) ->
	Start = now(),
	io:format("sync update cache ~p start~n", [Type]),
	Ret = do_update(Type),
	Val = {now(), Ret},
	io:format("sync update cache ~p done used ~p ms~n", [Type, 
		timer:now_diff(now(), Start) div 1000]),
	NewCache = gb_trees:enter(Type, Val, Cache),
	case gb_trees:size(NewCache) >= ?CACHE_SIZE of
		true -> 
			gen_server:cast(self(), decrease_cache);
		false ->
			ok
	end,
	{State#state{cache = NewCache}, Ret}.

do_update({search, Key}) ->
	db_frontend:search(Key);

do_update(top) ->
	db_frontend:today_top();

do_update(stats) ->
	db_frontend:stats().

do_query(Type, #state{cache = Cache} = State) ->
	{Start, Ret} = gb_trees:get(Type, Cache),
	NewCache = case is_outofdate(Start) of
		true ->
			spawn_update(Type),
			% update the time so that it will not be scheduled more time
			gb_trees:enter(Type, {now(), Ret}, Cache);
		false ->
			Cache
	end,
	{State#state{cache = NewCache}, Ret}.

is_outofdate(Time) ->
	(timer:now_diff(now(), Time) div 1000) > ?OUT_OF_DATE.

remove_oldest(Cache) ->
	io:format("decrease cache ~p to ~p~n", [?CACHE_SIZE, ?CACHE_SIZE div 2]),
	KeyVals = gb_trees:to_list(Cache),
	Sorted = lists:sort(fun(A, B) -> compare_keyval(A, B) end, KeyVals),
	{_, Rest} = lists:split(?CACHE_SIZE div 2, Sorted),
	lists:foldl(fun({Key, Val}, Tree) ->
		gb_trees:insert(Key, Val, Tree)
	end, gb_trees:empty(), Rest).

compare_keyval({_, {T1, _}}, {_, {T2, _}}) ->
	T1 =< T2.

spawn_update(Type) ->
	spawn(?MODULE, async_update, [Type, self()]).

async_update(Type, From) ->
	Start = now(),
	io:format("async update cache ~p start~n", [Type]),
	Ret = do_update(Type),
	From ! {enter_cache, Type, now(), Ret},
	io:format("async update cache ~p done used ~p ms~n", 
		[Type, timer:now_diff(now(), Start) div 1000]).

