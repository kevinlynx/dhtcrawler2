%%
%% hash_reader_sup.erl
%% Kevin Lynx
%% 06.29.2013
%%
-module(hash_reader_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/3,
		 start_dep_apps/0,
		 start_standalone/3,
		 start_standalone/1]).
-define(DBPOOLNAME, mongodb_conn_pool_name).
		 
start_dep_apps() ->
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/mongodb/ebin"),
	code:add_path("deps/kdht/ebin"),
	code:add_path("deps/ibrowse/ebin"),
	Apps = [asn1, crypto, public_key, ssl, inets, bson, mongodb],	
	[application:start(App) || App <- Apps].

start_standalone([IP, Port, Size]) ->
	IPort = list_to_integer(Port),
	ISize = list_to_integer(Size),
	start_standalone(IP, IPort, ISize),
	receive 
		fuck_erl_s_option -> ok
	end.

start_standalone(IP, Port, Size) ->
	io:format("db: ~p:~p reader count ~p~n", [IP, Port, Size]),
	filelib:ensure_dir("log/"),
	start_dep_apps(),
	tor_download:start_global(),
	config:start_link("hash_reader.config", fun() -> config_default() end),
	tor_name_seg:init(),
	% NOTE:
	Stats = {hash_reader_stats, {hash_reader_stats, start_link, [Size]}, permanent, 2000, worker, [hash_reader_stats]},
	DownloadStats = {tor_download_stats, {tor_download_stats, start_link, []}, permanent, 2000, worker, [tor_download_stats]},
	Log = {vlog, {vlog, start_link, ["log/hash_reader.log", 3]}, permanent, 2000, worker, [vlog]},
	DBDateRange = {db_daterange, {db_daterange, start_link, [?DBPOOLNAME]}, permanent, 1000, worker, [db_daterange]},
	start_link(IP, Port, Size, [Log, DBDateRange, DownloadStats, Stats]).

start_link(IP, Port, Size) ->
	start_link(IP, Port, Size, []).

start_link(IP, Port, Size, OtherProcess) ->
	PoolName = ?DBPOOLNAME,
	mongo_sup:start_pool(PoolName, 5, {IP, Port}),
	% ensure index
	Conn = mongo_pool:get(PoolName),
	db_store_mongo:init(Conn),
	supervisor:start_link({local, srv_name()}, ?MODULE, [PoolName, Size, OtherProcess]).

srv_name() ->
	?MODULE.

init([PoolName, Size, OtherProcess]) ->
	Spec = {one_for_one, 1, 600},
	Children = OtherProcess ++ [create_child(PoolName, Index) || Index <- lists:seq(1, Size)],
    {ok, {Spec, Children}}.

create_child(PoolName, Index) ->
	{child_id(Index), {hash_reader, start_link, [PoolName]}, 
		permanent, 1000, worker, dynamic}.

child_id(Index) ->
	list_to_atom(lists:flatten(io_lib:format("hash_reader_~p", [Index]))).

config_default() ->
	[{save_torrent, true},
	 {save_to_db, false},
	 {save_to_file, true},
	 {load_from_db, false},
	 {text_seg, simple},
	 {check_cache, false},
	 {torrent_path, "torrents/"}].
