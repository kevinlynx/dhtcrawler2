%%
%% tor_builder.erl
%% Kevin Lynx
%% 07.07.2013
%%
-module(tor_builder).
-include("vlog.hrl").
-export([start_standalone/1, start_standalone/4]).
-define(POOLNAME, write_tor_dbpool).
-define(BATCHSIZE, 10).
-compile(export_all).

start_dep_apps() ->
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/mongodb/ebin"),
	code:add_path("deps/kdht/ebin"),
	Apps = [asn1, crypto, public_key, ssl, inets, bson, mongodb],	
	[application:start(App) || App <- Apps].

start_standalone([IP, Port, Size, RootDir]) ->
	IPort = list_to_integer(Port),
	ISize = list_to_integer(Size),
	start_standalone(IP, IPort, ISize, RootDir),
	receive 
		fuck_erl_s_option -> ok
	end.

start_standalone(IP, Port, PoolSize, RootDir) ->
	start_dep_apps(),
	mongo_sup:start_pool(?POOLNAME, PoolSize, {IP, Port}),
	vlog:start_link("tor_builder.log", 1),
	Dirs = get_dir_list(RootDir),
	?I(?FMT("found ~p subdirectories in ~s", [length(Dirs), RootDir])),
	[start(?POOLNAME, RootDir ++ Dir) || Dir <- Dirs],
	ok.

get_dir_list(RootDir) ->
	{ok, Dirs} = file:list_dir(RootDir),
	lists:dropwhile(fun (S) -> filter_dir(S) end, Dirs).

filter_dir([$\.|_]) ->
	true;
filter_dir(_) ->
	false.
%%
start(PoolName, Dir) ->
	spawn(?MODULE, load_and_save, [PoolName, Dir]).

load_and_save(PoolName, Dir) ->
	Now = now(),
	?T(?FMT("~p startup", [self()])),
	Names = load_torrent_names(Dir),
	?I(?FMT("~p found ~p torrents in dir ~s, used ~p ms", [self(), length(Names), Dir,
		timer:now_diff(now(), Now) div 1000])),
	parse_and_save(PoolName, [], Names),
	io:format("~p parsed and saved ~p torrents~n", [self(), length(Names)]),
	?I(?FMT("save torrents done ~p", [self()])).

load_torrent_names(Dir) ->
	Files = filelib:wildcard(Dir ++ "/**/*.torrent"),
	Files.

parse_and_save(PoolName, Docs, [Name|Files]) when 1 + length(Docs) == ?BATCHSIZE ->
	SaveDocs = load_and_parse(Name, Docs),
	try_save(PoolName, SaveDocs),
	parse_and_save(PoolName, [], Files);
	
parse_and_save(PoolName, Docs, [Name|Files]) ->
	SaveDocs = load_and_parse(Name, Docs),
	parse_and_save(PoolName, SaveDocs, Files);	
	
parse_and_save(PoolName, Docs, []) ->
	try_save(PoolName, Docs).

load_and_parse(Name, AccIn) ->
	{ok, Content} = file:read_file(Name),
	MagHash = parse_hash(Name),
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

try_save(_, []) ->
	ok;
try_save(PoolName, Tors) ->
	Conn = mongo_pool:get(PoolName),
	db_store_mongo:unsafe_insert(Conn, Tors).

