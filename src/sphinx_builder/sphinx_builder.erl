%%
%% sphinx_builder.erl
%% Kevin Lynx
%% 07.29.2013
%%
-module(sphinx_builder).
-include("vlog.hrl").
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/3]).
-export([worker_run/0]).
-record(state, {processed = 0, saved = false, worker_cnt, wait_workers = []}).
-define(WORKER_WAIT, 30*1000).
-define(STATE_FILE, "priv/sphinx_builder.sta").

start_link(IP, Port, Count) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [IP, Port, Count], []).

srv_name() ->
	?MODULE.

init([IP, Port, WorkerCnt]) ->
	?I(?FMT("spawn ~p workers", [WorkerCnt])),
	[spawn_link(?MODULE, worker_run, []) || _ <- lists:seq(1, WorkerCnt)],
	{Offset, Date} = load_result(),
	sphinx_torrent:start_link(IP, Port, Date),
	{ok, #state{processed = Offset, worker_cnt = WorkerCnt}}.

handle_call({get, Pid}, _From, State) ->
	#state{processed = Processed, worker_cnt = WorkerCnt, wait_workers = WaitWorkers,
		saved = Saved} = State,
	{NewProcessed, Ret} = case sphinx_torrent:get() of
		{} -> 
			{Processed, wait};
		Tor -> 
			check_progress(Processed + 1),
			{Processed + 1, Tor}
	end,
	NewWaits = update_wait_workers(Pid, NewProcessed, Processed, WaitWorkers),
	NewSaved = (check_all_done(NewWaits, WorkerCnt, NewProcessed, Saved)) and (Ret == wait),
	{reply, {NewProcessed, Ret}, State#state{processed = NewProcessed, wait_workers = NewWaits,
		saved = NewSaved}}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

update_wait_workers(Pid, NewProcessed, Processed, WaitWorkers) ->
	case lists:member(Pid, WaitWorkers) of
		true when NewProcessed > Processed ->
			lists:delete(Pid, WaitWorkers);
		false when NewProcessed == Processed ->
			[Pid|WaitWorkers];
		_ -> 
			WaitWorkers
	end.

check_all_done(WaitWorkers, WorkerCnt, Processed, false) 
when length(WaitWorkers) == WorkerCnt ->
	Try = sphinx_torrent:try_times(),
	case Try > 5 of 
		true ->
			io:format("haven't got any torrents for a while, force save~n", []),
			save_result(Processed),
			sphinx_xml:force_save(),
			true;
		false ->
			false
	end;
check_all_done(_WaitWorkers, _WaitCnt, _Processed, Saved) ->
	Saved.

worker_run() ->
	Ret = gen_server:call(srv_name(), {get, self()}, infinity),
	do_process(Ret),
	worker_run().

do_process({_, wait}) ->
	timer:sleep(?WORKER_WAIT);
do_process({ID, Doc}) ->
	case db_store_mongo:decode_torrent_item(Doc) of
		{single, Hash, {Name, _}, Query, CreatedAt} ->
			sphinx_xml:insert({ID, Hash, Name, [], Query, CreatedAt});
		{multi, Hash, {Name, Files}, Query, CreatedAt} ->
			sphinx_xml:insert({ID, Hash, Name, Files, Query, CreatedAt})
	end.

load_result() ->
	case file:consult(?STATE_FILE) of
		{error, _Reason} ->
			io:format("start a new processing~n", []),
			{0, 0};
		{ok, [Ret]} ->
			Sum = proplists:get_value(processed, Ret),
			DateSecs = proplists:get_value(offset_date, Ret),
			io:format("continue to process from ~s~n", [time_util:date_time_string(DateSecs)]),
			{Sum, DateSecs}
	end.

save_result(Sum) ->
	DateSecs = sphinx_torrent:offset_date(),
	Ret = [{processed, Sum}, {offset_date, DateSecs}],
	io:format("save result ~s~n", [time_util:date_time_string(DateSecs)]),
	file:write_file(?STATE_FILE, io_lib:fwrite("~p.\n",[Ret])).

check_progress(Sum) ->
	case (Sum rem 500 == 0) and (Sum > 0) of
		true ->
			save_result(Sum),
			io:format(" -> ~p~n", [Sum]);
		false ->
			ok
	end.
