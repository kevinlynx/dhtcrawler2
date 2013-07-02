%%
%% crawler_sup.erl
%% Kevin Lynx
%% 06.14.2013
%%
-module(crawler_sup).
-behaviour(supervisor).
-include("vlog.hrl").
-export([init/1]).
-export([start_link/1,
		 stop/0,
		 save_all_state/0]).

start_link(Args) ->
	supervisor:start_link({local, srv_name()}, ?MODULE, [Args]).

stop() ->
	save_all_state(),
	exit(whereis(srv_name()), normal).

srv_name() ->
	dht_crawler_sup.

init([{StartPort, Count, DBHost, DBPort, LogLevel, DBConn, HashCacheMax}]) ->
	Spec = {one_for_one, 1, 600},
	Instances = create_dht_instance(StartPort, Count),
	Logger = [{dht_logger, {vlog, start_link, ["dht_crawler.txt", LogLevel]},
					permanent, 2000, worker, dynamic}],
	%Downloader = [{torrent_downloader, {torrent_download, start_link, []},
	%				permanent, 2000, worker, dynamic}],
	%DBStorer = [{torrent_index, {torrent_index, start_link, [DBHost, DBPort, crawler_stats, DBConn]},
	%				permanent, 2000, worker, dynamic}],
	HashInserter = [{db_hash, {db_hash, start_link, [DBHost, DBPort, DBConn, HashCacheMax]},
					permanent, 2000, worker, dynamic}],
	Stats = [{crawler_stats, {crawler_stats, start_link, []},
					permanent, 2000, worker, dynamic}],
	%Children = Logger ++ Downloader ++ DBStorer ++ Instances,
	Children = Logger ++ HashInserter ++ Stats ++ Instances,
    {ok, {Spec, Children}}.

create_dht_instance(StartPort, Count) ->
	Dir = "dhtstate/",
	filelib:ensure_dir(Dir),
	IDs = create_discrete_ids(Count),
	Generator = lists:zip(IDs, lists:seq(StartPort, StartPort + Count - 1)),
	[{instance_name(Port),
		{kdht_sup, start_link, [instance_state_file(Dir, Port), Port, dht_monitor, ID]},
		permanent, 1000, supervisor, [kdht]} 
		|| {ID, Port} <- Generator].

instance_name(Port) ->
	Name = lists:flatten(io_lib:format("dht_instance_~p", [Port])),
	list_to_atom(Name).

instance_state_file(Dir, Port) ->
	lists:flatten(io_lib:format("~sstate~b", [Dir, Port])).

create_discrete_ids(1) ->
	[dht_id:random()];

create_discrete_ids(Count) ->
	Max = dht_id:max(),
	Piece = Max div Count,
	[random:uniform(Piece) + Index * Piece || Index <- lists:seq(0, Count - 1)].

save_all_state() ->
	[save_instance_state(Instance) || Instance <- supervisor:which_children(srv_name())].

save_instance_state({_ID, Pid, supervisor, [kdht]}) ->
	kdht_sup:save_state(Pid);

save_instance_state({_ID, _Pid, worker, _}) ->
	ok.


