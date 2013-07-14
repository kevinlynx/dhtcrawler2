%%
%% name_seger.erl
%% Kevin Lynx
%% segment name by rmmseg 
%%
-module(name_seger).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start/0, start/1, start/3, worker_run/0, loader_run/1]).
-define(DBNAME, torrents).
-define(COLLNAME, hashes).
-define(POOLNAME, db_pool).
-define(BATCHSIZE, 300).
-record(state, {sum = 0, workers = [], loader = undefined, torcache = []}).

start_dep_apps() ->
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/mongodb/ebin"),
	Apps = [asn1, crypto, public_key, ssl, inets, bson, mongodb],	
	[application:start(App) || App <- Apps].

start() ->
	start(50).

start(Count) ->
	start_dep_apps(),
	start_link(localhost, 27017, Count).

start(IP, Port, Count) ->
	start_dep_apps(),
	start_link(IP, Port, Count).

start_link(IP, Port, Count) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [IP, Port, Count], []).

srv_name() ->
	?MODULE.
%
init([IP, Port, Count]) ->
	rmmseg:init(),
	rmmseg:load_dicts(),
	{ok, {IP, Port, Count}, 0}.

handle_cast(stop, State) ->
	save_result(State#state.sum),
	{stop, normal, State}.

handle_call({append, Tors}, From, State) ->
	#state{torcache = MyTors} = State,
	io:format("loader append ~p ~n", [length(Tors)]),
	NewTors = MyTors ++ Tors,
	case length(NewTors) > ?BATCHSIZE * 2 of
		true -> % too fast
			{noreply, State#state{loader = From, torcache = NewTors}};
		false ->
			{reply, ok, State#state{torcache = NewTors}}
	end;

handle_call({worker_done, Worker}, _From, State) ->
	#state{sum = Sum, workers = Workers} = State,
	NewWorkers = lists:delete(Worker, Workers),
	case length(NewWorkers) of 
		0 ->
			io:format("process done total ~p~n", [Sum]),
			save_result(Sum),
			{stop, normal, State#state{workers = []}};
		_ ->	
			{reply, ok, State#state{workers = NewWorkers}}
	end;

handle_call(loader_done, _From, State) ->
	% set a flag and wait workers done
	{reply, ok, State#state{loader = loader_done}};

handle_call(get_one, _From, #state{torcache = Tors} = State) 
when length(Tors) == 0 ->
	#state{loader = Loader} = State,
	Ret = if Loader == loader_done -> exit; true -> wait end,
	{reply, Ret, State};

handle_call(get_one, _From, State) ->
	#state{sum = Sum, torcache = Tors, loader = Loader} = State,
	print_stats(Sum),
	[This|Rest] = Tors,
	RestSize = length(Rest),
	NewLoader = notify_loader(RestSize, Loader),
	{reply, This, State#state{torcache = Rest, sum = 1 + Sum, loader = NewLoader}}.

notify_loader(_, loader_done) ->
	loader_done;
notify_loader(Size, Loader) when Loader /= undefined, Size < ?BATCHSIZE ->
	gen_server:reply(Loader, continue),
	undefined;
notify_loader(_, Loader) ->
	Loader.	

handle_info(timeout, {IP, Port, Count}) ->
	Sum = load_result(),
	mongo_sup:start_pool(?POOLNAME, 5, {IP, Port}),
	Workers = [spawn_link(?MODULE, worker_run, []) || _ <- lists:seq(1, Count)],
	spawn_link(?MODULE, loader_run, [Sum]),
	{noreply, #state{sum = Sum, workers = Workers}};

handle_info(_, State) ->
	{noreply, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

loader_run(Skip) ->
	Tors = load_torrents_with_conn(Skip),
	case length(Tors) of
		0 ->
			io:format("loader exit~n", []),
			gen_server:call(srv_name(), loader_done),
			ok;
		Size ->
			gen_server:call(srv_name(), {append, Tors}, infinity),
			loader_run(Skip + Size)
	end.

%%
load_result() ->
	case file:consult("nameseg.txt") of
		{error, _Reason} ->
			io:format("start a new processing~n", []),
			0;
		{ok, [Ret]} ->
			Sum = proplists:get_value(processed, Ret),
			io:format("continue to process skip ~p~n", [Sum]),
			Sum
	end.

save_result(Sum) ->
	Ret = [{processed, Sum}],
	io:format("save result ~p~n", [Sum]),
	file:write_file("nameseg.txt", io_lib:fwrite("~p.\n",[Ret])).
	
load_torrents_with_conn(Skip) ->
	Conn = mongo_pool:get(?POOLNAME),
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		Cursor = mongo:find(?COLLNAME, {}, {}, Skip),
		load_torrents(Cursor)
	end).

load_torrents(Cursor) ->
	mongo_cursor:take(Cursor, ?BATCHSIZE).

worker_run() ->
	Doc = gen_server:call(srv_name(), get_one, infinity),
	Ret = do_process(Doc),
	case Ret of
		exit -> ok;
		_ ->
			worker_run()
	end.			

do_process(exit) ->
	gen_server:call(srv_name(), {worker_done, self()}),
	exit;
do_process(wait) ->
	timer:sleep(1000);
do_process(Doc) ->
	Torrent = db_store_mongo:decode_torrent_item(Doc),
	{Hash, NameArray} = seg_torrent(Torrent),
	Conn = mongo_pool:get(?POOLNAME),
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		commit(list_to_binary(Hash), NameArray)
	end).

%%
print_stats(Sum) ->
	case (Sum rem 500 == 0) and (Sum > 0) of
		true ->
			save_result(Sum),
			io:format(" -> ~p~n", [Sum]);
		false ->
			ok
	end.

commit(Hash, NameArray) when is_binary(Hash), is_binary(NameArray) ->
	Cmd = {findAndModify, ?COLLNAME, query, {'_id', Hash}, 
		update, {'$set', {name_array, NameArray}}, fields, {'_id', 1},
		new, false},
	mongo:command(Cmd).

seg_torrent({single, Hash, {Name, _}, _, _}) ->
	{Hash, rmmseg:seg_space(list_to_binary(Name))};

seg_torrent({multi, Hash, {Name, _Files}, _, _}) ->
	%FullName = lists:foldl(fun({S, _}, Acc) -> Acc ++ " " ++ S end, Name, Files),
	{Hash, rmmseg:seg_space(list_to_binary(Name))}.

