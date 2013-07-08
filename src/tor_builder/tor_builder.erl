%%
%% tor_builder.erl
%% Kevin Lynx
%% 07.08.2013
%%
-module(tor_builder).
-behaviour(gen_server).
-include("vlog.hrl").
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([worker_run/1]).
-export([start_link/4,
		 start_link/1,
		 start_standalone/1,
		 start_standalone/4,
		 stop/0]).
-record(state, {dir, workers = [], workercnt}).
-define(DBPOOL, tor_db_pool).
-define(BATCHSIZE, 10).

start_dep_apps() ->
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/mongodb/ebin"),
	code:add_path("deps/kdht/ebin"),
	Apps = [asn1, crypto, public_key, ssl, inets, bson, mongodb],	
	[application:start(App) || App <- Apps].

% called from shell
start_standalone([IP, Port, WorkerCount, RootDir]) ->
	IPort = list_to_integer(Port),
	IWorkerCount = list_to_integer(WorkerCount),
	start_standalone(IP, IPort, IWorkerCount, RootDir),
	receive 
		fuck_erl_s_option -> ok
	end.

% `RootDir' must follow a slash e.g: torrents/
start_standalone(IP, Port, WorkerCount, RootDir) ->
	start_dep_apps(),
	start_link(IP, Port, RootDir, WorkerCount).

%%
start_link(DBIP, DBPort, RootDir, WorkerCount) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [DBIP, DBPort, RootDir, WorkerCount], []).

start_link(RootDir) ->
	start_link(localhost, 27017, RootDir, 100).

stop() ->
	gen_server:cast(srv_name, stop).

srv_name() ->
	?MODULE.

init([DBIP, DBPort, RootDir, WorkerCount]) ->
	mongo_sup:start_pool(?DBPOOL, 5, {DBIP, DBPort}),
	vlog:start_link("tor_builder.log", 1),
	{ok, #state{dir = RootDir, workercnt = WorkerCount}, 1}.

code_change(_, _, State) ->
    {ok, State}.

terminate(_, State) ->
	tor_location_reader:stop(),
	mongo_sup:stop_pool(?DBPOOL),
	vlog:stop(),
    {ok, State}.

handle_info(build_index_done, State) ->
	#state{workercnt = WorkerCount, dir = RootDir} = State,
	Workers = spawn_workers(RootDir, WorkerCount),
	{noreply, State#state{workers = Workers}};

handle_info({worker_done, Pid}, State) ->
	#state{workers = Workers} = State,
	NewWorks = lists:delete(Pid, Workers),
	case length(NewWorks) of
		0 ->
			io:format("import all torrents done~n"),
			{stop, normal, State#state{workers = NewWorks}};
		_ ->
			{noreply, State#state{workers = NewWorks}}
	end;

handle_info(timeout, State) ->
	#state{dir = RootDir, workercnt = WorkerCount} = State,
	Workers = case tor_location_reader:ensure_status_files(RootDir) of
		true ->
			io:format("continue to process...~n"),
			spawn_workers(RootDir, WorkerCount);
		false ->
			tor_location_writer:start_link(RootDir, self()),
			[]
	end,
	{noreply, State#state{workers = Workers}};

handle_info(M, State) ->
	?W(?FMT("unhandled message ~p", [M])),
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

spawn_workers(RootDir, Count) ->
	tor_location_reader:start_link(RootDir),
	io:format("spawn ~p worker processes~n", [Count]),
	[spawn_link(?MODULE, worker_run, [self()]) || _ <- lists:seq(1, Count)].

worker_run(Parent) ->
	Names = tor_location_reader:get(?BATCHSIZE),
	?T(?FMT("read ~p torrent file names", [length(Names)])),
	case length(Names) == 0 of
		true ->
			process_done(Parent);
		false ->
			parse_and_save([], Names),
			worker_run(Parent)
	end.

process_done(Parent) ->
	Parent ! {worker_done, self()}.

parse_and_save(Docs, [Name|Files]) ->
	SaveDocs = load_and_parse(Name, Docs),
	parse_and_save(SaveDocs, Files);	
	
parse_and_save(Docs, []) ->
	try_save(Docs).

load_and_parse(Name, AccIn) ->
	{ok, Content} = file:read_file(Name),
	MagHash = parse_hash(Name),
	40 = length(MagHash),
	case catch(torrent_file:parse(Content)) of
		{'EXIT', _} ->
			?W(?FMT("parse a torrent failed ~s", [MagHash])),
			AccIn;
		{Type, Info} -> 
			Doc = on_load_torrent(MagHash, Type, Info),
			[Doc|AccIn]
	end.

parse_hash(FileName) ->
	string:to_upper(filename:basename(FileName, ".torrent")).

on_load_torrent(Hash, single, {Name, Length}) ->
	{Hash, Name, Length, []};

on_load_torrent(Hash, multi, {Root, Files}) ->
	{Hash, Root, 0, Files}.

try_save([]) ->
	ok;
try_save(Tors) ->
	Conn = mongo_pool:get(?DBPOOL),
	db_store_mongo:unsafe_insert(Conn, Tors).

