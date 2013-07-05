%%
%% loc_torrent_sup.erl
%% Kevin Lynx
%% 07.03.2013
%%
-module(loc_torrent_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/3,
		 start_dep_apps/0,
		 start_standalone/3,
		 start_standalone/1]).
		 
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
	io:format("loc torrent (torrent downloader) startup~n", []),
	io:format("db: ~p:~p downloader count ~p~n", [IP, Port, Size]),
	start_dep_apps(),
	tor_download:start_global(),
	config:start_link("torcache.config", fun() -> config_default() end),
	start_link(IP, Port, Size, []).

start_link(IP, Port, Size) ->
	OtherProcess = [],
	start_link(IP, Port, Size, OtherProcess).

start_link(IP, Port, Size, OtherProcess) ->
	PoolName = loc_torrent_db_pool,
	mongo_sup:start_pool(PoolName, 5, {IP, Port}),
	supervisor:start_link({local, srv_name()}, ?MODULE, [PoolName, Size, OtherProcess]).

srv_name() ->
	?MODULE.

init([PoolName, Size, OtherProcess]) ->
	Spec = {one_for_one, 1, 600},
	Children = OtherProcess ++ [create_child(PoolName, Index) || Index <- lists:seq(1, Size)],
    {ok, {Spec, Children}}.

create_child(PoolName, Index) ->
	{child_id(Index), {loc_torrent, start_link, [PoolName]}, 
		permanent, 1000, worker, dynamic}.

child_id(Index) ->
	list_to_atom(lists:flatten(io_lib:format("loc_torrent_~p", [Index]))).

config_default() ->
	[{save_to_db, false},
	 {save_to_file, true},
	 {torrent_path, "torrents/"}].


