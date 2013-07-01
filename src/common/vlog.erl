%%
%% vlog.erl
%% Kevin Lynx
%% 06.05.2013
%%
-module(vlog).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/2,
         format/3,
         set_level/1,
         sync_format/3,
         time_string/0,
         stop/0]).
-record(state, {name, level}).
-include("vlog.hrl").

start_link(Name, Lvl) ->
    gen_server:start_link({local, srv_name()}, ?MODULE, [Name, Lvl], []).

stop() ->
    gen_server:cast(srv_name(), stop).

set_level(Level) ->
    gen_server:call(srv_name(), {set_level, Level}).

format(Lvl, Fmt, Arg) ->
    gen_server:cast(srv_name(), {write, Lvl, Fmt, Arg}).

sync_format(Lvl, Fmt, Arg) ->
    gen_server:call(srv_name(), {write, Lvl, Fmt, Arg}).

time_string() ->
    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = erlang:localtime(),
    lists:flatten(io_lib:format("~2.10.0b:~2.10.0b:~2.10.0b", [Hour, Min, Sec])).

srv_name() ->
    ?MODULE.

init([Name, Lvl]) ->
    {ok, FP} = file:open(Name, [write]),
    file:close(FP),
    {ok, #state{name = Name, level = Lvl}}.

handle_info(_, State) ->
    {noreply, State}.

handle_cast({write, Level, Fmt, Arg}, State) ->
    do_log(State, Level, Fmt, Arg),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_call({write, Level, Fmt, Arg}, _From, State) ->
    do_log(State, Level, Fmt, Arg),
    {reply, ok, State};

handle_call({set_level, Level}, _From, State) ->
    {reply, ok, State#state{level = Level}};

handle_call(_, _From, State) ->
    {reply, not_implemented, State}.

do_log(State, Level, Fmt, Arg) ->
    #state{name = Name, level = MaxLevel} = State,
    case Level >= MaxLevel of
        true -> append(Name, Fmt, Arg);
        false -> false
    end.

append(Name, Fmt, Arg) ->
    {ok, FP} = file:open(Name, [append]),
    io:format(FP, Fmt, Arg),
    file:close(FP).

