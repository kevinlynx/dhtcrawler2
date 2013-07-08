%%
%% tor_location_writer.erl
%% Kevin Lynx
%% 07.08.2013
%%
-module(tor_location_writer).
-behaviour(gen_server).
-include("vlog.hrl").
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/2, stop/0]).
-export([worker_run/2]).
-record(state, {parent, dir, fp, sum = 0, workers = []}).
-compile(export_all).

start_link(RootDir, Parent) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [RootDir, Parent], []).

stop() ->
	gen_server:cast(srv_name(), stop).

srv_name() ->
	?MODULE.

init([RootDir, Parent]) ->
	{ok, FP} = file:open(RootDir ++ "index.txt", [write]),
	{ok, #state{parent = Parent, dir = RootDir, fp = FP}, 0}.

terminate(_, #state{fp = FP} = State) ->
	file:close(FP),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(timeout, State) ->
	#state{dir = RootDir} = State,
	Dirs = list_directory(RootDir),
	?I(?FMT("~s has ~p subdirectories", [RootDir, length(Dirs)])),
	Workers = [spawn_link(?MODULE, worker_run, [RootDir, Dir]) || Dir <- Dirs],
	io:format("start to build the torrent index.....~n", []),
	{noreply, State#state{workers = Workers}};

handle_info(M, State) ->
	?W(?FMT("unhandled message ~p", [M])),
	{noreply, State}.

handle_cast({worker_done, Pid}, State) ->
	#state{parent = Parent, workers = Workers, sum = Sum, dir = RootDir} = State,
	NewWorks = lists:delete(Pid, Workers),
	case length(NewWorks) of
		0 ->
			on_build_done(RootDir, Sum, Parent),
			{stop, normal, State#state{workers = NewWorks}};
		_ ->
			{noreply, State#state{workers = NewWorks}}
	end;

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call({add_torrents, Names}, _From, State) ->
	#state{fp = FP, sum = Sum} = State,
	AddCnt = append_torrent_names(FP, Names),
	?I(?FMT("append ~p torrents to index file", [AddCnt])),
	{reply, ok, State#state{sum = Sum + AddCnt}};

handle_call(_, _From, State) ->
	{noreply, State}.

on_build_done(RootDir, Sum, Parent) ->
	?I(?FMT("build index done, total ~p torrents", [Sum])),
	io:format("build index done, total ~p torrents~n", [Sum]),
	Ret = [{sum, Sum}, {position, 0}],
	file:write_file(RootDir ++ "index_status.txt", io_lib:fwrite("~p.\n",[Ret])),
	Parent ! build_index_done.

list_directory(RootDir) ->
	{ok, Files} = file:list_dir(RootDir),
	% dropwhile not work??
	lists:filter(fun (S) -> not filter_dir(RootDir ++ S) end, Files).

filter_dir([$\.|_]) ->
	true;
filter_dir(Dir) ->
	(not filelib:is_file(Dir)) or (filelib:is_regular(Dir)).

parse_hash(FileName) ->
	string:to_upper(filename:basename(FileName, ".torrent")).

append_torrent_names(FP, [Name|Rest]) ->
	Hash = parse_hash(Name),
	case length(Hash) == 40 of
		true ->	
			io:format(FP, "~s~n", [Name]) ,
			1 + append_torrent_names(FP, Rest);
		false ->
			?W(?FMT("invalid torrent file name ~s", [Name]))
	end;

append_torrent_names(_, []) ->
	0.

%%
notify_worker_done() ->
	gen_server:cast(srv_name(), {worker_done, self()}).

notify_add_torrents([]) ->
	skip;

notify_add_torrents(Names) ->
	gen_server:call(srv_name(), {add_torrents, Names}, infinity).

load_torrent_names(Dir) ->
	Files = filelib:wildcard(Dir ++ "/*.torrent"),
	Files.

worker_run(RootDir, Dir) ->
	FullDir = RootDir ++ Dir ++ "/",
	SubDirs = list_directory(FullDir),
	?I(?FMT("worker ~p has ~p subdirectories", [self(), length(SubDirs)])),
	process_torrents(FullDir, SubDirs),
	notify_worker_done().

process_torrents(ParentDir, [Dir|Rest]) ->
	FullDir = ParentDir ++ "/" ++ Dir,
	Names = load_torrent_names(FullDir),
	?I(?FMT("worker ~p subdirectory ~s has ~p torrents", [self(), FullDir, length(Names)])),
	notify_add_torrents(Names),
	process_torrents(ParentDir, Rest);

process_torrents(_, []) ->
	ok.




