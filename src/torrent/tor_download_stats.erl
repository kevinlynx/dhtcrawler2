%%
%% tor_download_stats.erl
%% Kevin Lynx
%% 06.30.2013
%%
-module(tor_download_stats).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/0,
		 stop/0,
		 register/1,
		 stats/0]).
-record(state, {pids = []}).

start_link() ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

stop() ->
	gen_server:cast(srv_name(), stop).

% {ProcessCount, HashSum, ReqSum, TotalTime, CurrentReqCount}
stats() ->
	gen_server:call(srv_name(), stats, infinity).

register(Pid) ->
	gen_server:cast(srv_name(), {register, Pid}).

srv_name() ->
	?MODULE.

%%
init([]) ->
	{ok, #state{}}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(_, State) ->
	{noreply, State}.

handle_cast({register, Pid}, State) ->
	#state{pids = Pids} = State,
	{noreply, State#state{pids = [Pid|Pids]}};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(stats, _From, State) ->
	#state{pids = Pids} = State,
	{reply, stats(Pids), State};

handle_call(_, _From, State) ->
	{noreply, State}.

stats(Pids) ->
	% {ProcessCount, HashSum, ReqSum, TotalTime, CurrentReqCount}
	Info = {0, 0, 0, 0, 0},
	lists:foldl(fun(Pid, {PSum, HashSum, ReqSum, TimeSum, CurReqCount}) ->
		{ThisH, ThisReq, ThisTime, ThisCurReq} = tor_download:stats(Pid),
		{PSum + 1, HashSum + ThisH, ReqSum + ThisReq, TimeSum + ThisTime, CurReqCount + ThisCurReq}
	end, Info, Pids).

