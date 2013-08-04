%%
%% sphinx_builder_sup.erl
%% Kevin Lynx
%% 07.28.2013
%%
-module(sphinx_builder_sup).
-behaviour(supervisor).
-export([start_standalone/1, start_standalone/3, start_link/3]).
-export([init_indexes/0]).
-export([init/1]).

start_dep_apps() ->
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/mongodb/ebin"),
	Apps = [asn1, crypto, public_key, ssl, inets, xmerl, bson, mongodb],	
	[application:start(App) || App <- Apps].

start_standalone([IP, Port, Count]) ->
	IPort = list_to_integer(Port),
	ISize = list_to_integer(Count),
	start_standalone(IP, IPort, ISize),
	receive 
		fuck_erl_s_option -> ok
	end.

start_standalone(IP, Port, Size) -> 
	start_dep_apps(),
	start_link(IP, Port, Size).

start_link(IP, Port, Count) ->
	supervisor:start_link({local, srv_name()}, ?MODULE, [IP, Port, Count]).

init_indexes() ->
	config:start_link("sphinx_builder.config", fun() -> config_default() end),
	io:format("try init sphinx index files~n", []),
	Conf = config:get(sphinx_config_file),
	MainFile = config:get(main_source_file),
	DeltaFile = config:get(delta_source_file),
	sphinx_cmd:build_init_index(MainFile, DeltaFile, Conf).
%%
srv_name() ->
	?MODULE.

init([IP, Port, Count]) ->
	Spec = {one_for_one, 1, 600},
	config:start_link("sphinx_builder.config", fun() -> config_default() end),
	Builder = {sphinx_builder, {sphinx_builder, start_link, [IP, Port, Count]}, permanent, 1000, worker, [sphinx_builder]},
	Indexer = {sphinx_xml, {sphinx_xml, start_link, []}, permanent, 1000, worker, [sphinx_xml]},
	Logger = {vlog, {vlog, start_link, ["log/sphinx_build.log", 0]}, permanent, 1000, worker, [vlog]},
	Children = [Logger, Builder, Indexer],
    {ok, {Spec, Children}}.

config_default() ->
	[{max_doc_per_file, 1000},
	 {torrent_batch_count, 100},
	 {main_source_file, "var/source/main.xml"},
	 {delta_source_file, "var/source/delta.xml"},
	 {sphinx_config_file, "var/etc/csft.conf"},
	 {delta_index_name, "delta"},
	 {main_index_name, "main"}].
