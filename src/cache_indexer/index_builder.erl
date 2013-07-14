%%
%% index_builder.erl
%% Kevin Lynx
%% 07.14.2013
%%
-module(index_builder).
-include("vlog.hrl").
-compile(export_all).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/2, 
		 start_standalone/1,
		 start_standalone/2,
		 stop/0]).
-record(state, {work_on = [], done = [], workers = []}).
-define(WORKDIR, "sync/").
-define(DBPOOL, index_builder_pool).
-define(SYNC_TODAY_INTERVAL, 5*60*1000).

start_dep_apps() ->
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/mongodb/ebin"),
	code:add_path("deps/ibrowse/ebin"),
	Apps = [asn1, crypto, public_key, ssl, inets, bson, mongodb],	
	[application:start(App) || App <- Apps],
	ibrowse:start().

start_standalone([IP, Port]) ->
	IPort = list_to_integer(Port),
	start_standalone(IP, IPort),
	receive 
		fuck_erl_s_option -> ok
	end.

start_standalone(DBIP, DBPort) ->
	start_dep_apps(),
	start_link(DBIP, DBPort).

start_link(DBIP, DBPort) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [DBIP, DBPort], []).

stop() ->
	gen_server:cast(srv_name(), stop).

srv_name() ->
	?MODULE.

init([DBIP, DBPort]) ->
	mongo_sup:start_pool(?DBPOOL, 5, {DBIP, DBPort}),
	filelib:ensure_dir("log/"),
	vlog:start_link("log/hash_cache.txt", 0),
	{Done, WorkOn} = load_status(?WORKDIR),
	?I(?FMT("done ~p, workon ~p", [Done, WorkOn])),
	NewWorkOn = intersect_new_files(?WORKDIR, Done ++ WorkOn),
	?I(?FMT("new workon ~p", [NewWorkOn])),
	save_status(?WORKDIR, Done, NewWorkOn),
	{ok, #state{work_on = WorkOn ++ NewWorkOn, done = Done}, 0}.

terminate(_, State) ->
	mongo_sup:stop_pool(?DBPOOL),
	vlog:stop(),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(sync_today, State) ->
	#state{work_on = WorkOn} = State,
	FileName = index_download:today_file_name(),
	case lists:member(FileName, WorkOn) of
		true ->
			% the file is processing, we should wait
			?I(?FMT("today index file ~s is processing, wait", [FileName])),
			schedule_update_today();
		false ->
			?I(?FMT("start to download today index file ~s", [FileName])),
			index_download:download()
	end,
	{noreply, State};

handle_info({sync_torrent_index, ok, FileName}, State) ->
	#state{workers = Workers, work_on = WorkOn} = State,
	schedule_update_today(),
	?I(?FMT("today index file ~s download success", [FileName])),
	Pid = start_worker(FileName),
	{noreply, State#state{work_on = [FileName|WorkOn], workers = [Pid|Workers]}};

handle_info({sync_torrent_index, failed, FileName}, State) ->
	?W(?FMT("today index file ~s download failed", [FileName])),
	schedule_update_today(),
	{noreply, State};

handle_info({worker_done, Pid, FileName}, State) ->
	?I(?FMT("worker ~s done", [FileName])),
	#state{workers = Workers, done = Done, work_on = WorkOn} = State,
	NewWorkers = lists:delete(Pid, Workers),
	case length(NewWorkers) of
		0 ->
			?I("all index files have been done"),
			io:format("all index files have been done~n", []);
		_ ->
			ok
	end,
	NewDone = [FileName|Done],
	NewWorkOn = lists:delete(FileName, WorkOn),
	save_status(?WORKDIR, NewDone, NewWorkOn),
	{noreply, State#state{workers = NewWorkers, done = NewDone, work_on = NewWorkOn}};

handle_info(timeout, State) ->
	#state{work_on = WorkOn} = State,
	Workers = [start_worker(FileName) || FileName <- WorkOn],
	schedule_update_today(),
	{noreply, State#state{workers = Workers}}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{reply, not_implemented, State}.

schedule_update_today() ->
	timer:send_after(?SYNC_TODAY_INTERVAL, sync_today).

%%	
start_worker(FileName) ->
	Conn = mongo_pool:get(?DBPOOL),
	index_file:start(Conn, FileName).

intersect_new_files(Dir, Processed) ->
	Files = index_file_list(Dir),
	lists:foldl(fun(F, Acc) ->
		case lists:member(F, Processed) of
			true -> Acc;
			false -> [F|Acc]
		end
	end, [], Files).

load_status(Dir) ->
	case file:consult(Dir ++ "index.sta") of
		{ok, [Status]} ->
			Done = proplists:get_value(processed, Status),
			WorkOn = proplists:get_value(processing, Status),
			{Done, WorkOn};
		{error, _} ->
			{[], []}	
	end.

save_status(Dir, Done, WorkOn) ->
	Status = [{processed, Done}, {processing, WorkOn}],
	file:write_file(Dir ++ "index.sta", io_lib:fwrite("~p.\n",[Status])).

index_file_list(Dir) ->
	Files = filelib:wildcard(Dir ++ "*.txt"),
	Files.

