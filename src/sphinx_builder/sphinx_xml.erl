%%
%% sphinx_xml.erl
%% Kevin Lynx
%% Write sphinx xml file
%%
-module(sphinx_xml).
-behaviour(gen_server).
-compile(export_all).
-include("vlog.hrl").
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/0, insert/1, force_save/0]).
-record(state, {docs = [], ids = [], max}).

start_link() ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

insert(Doc) ->
	gen_server:call(srv_name(), {insert, Doc}, infinity).

force_save() ->
	gen_server:cast(srv_name(), save).

srv_name() ->
	?MODULE.

%%
init([]) ->
	Max = config:get(max_doc_per_file, 1000),
	{ok, #state{max = Max}}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast(save, #state{docs = []} = State) ->
	{noreply, State};

handle_cast(save, #state{docs = Docs, ids = IDs} = State) when length(Docs) > 0 ->
	try_save(Docs, 0, IDs),
	{noreply, State#state{docs = [], ids = []}};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call({insert, DocT}, _From, State) ->
	#state{docs = Docs, ids = IDs, max = Max} = State,
	{ID, Doc} = create_doc(DocT),
	{NewDocs, NewIDs} = try_save([Doc|Docs], Max, [ID|IDs]),
	{reply, ok, State#state{docs = NewDocs, ids = NewIDs}};

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

try_save(Docs, Max, IDs) when length(Docs) >= Max, length(Docs) > 0 ->
	File = config:get(delta_source_file),
	Conf = config:get(sphinx_config_file),
	Delta = config:get(delta_index_name),
	Main = config:get(main_index_name),
	{StartID, EndID} = get_id_range(IDs),
	io:format("sync sphinx index ~p documents...", [length(Docs)]),
	?I(?FMT("save sphinx xml file ~s", [File])),
	sphinx_doc:write_xml(File, Docs),
	?I(?FMT("build delta index : ~s", [Delta])),
	sphinx_cmd:build_delta_index(File, Delta, Conf, StartID, EndID),
	?I(?FMT("merge delta to main index ~s -> ~s", [Delta, Main])),
	sphinx_cmd:merge_index(Main, Delta, Conf),
	?I("index updated done"),
	io:format("done~n", []),
	{[], []};
try_save(Docs, _, IDs) ->
	{Docs, IDs}.

get_id_range([First|IDs]) ->
	lists:foldl(fun(ID, {Min, Max}) ->
		{min(ID, Min), max(ID, Max)}
	end, {First, First}, IDs).

create_doc({ID, Hash, Name, Files, Query, CreatedAt}) ->
	ValidName = valid_name(Name),
	ValidFiles = valid_file_names(Files),
	Doc = sphinx_doc:element(Hash, ValidName, ValidFiles, ID, Query, CreatedAt),
	{ID, Doc}.

valid_file_names(Files) ->
	[{valid_name(Name), Length} || {Name, Length} <- Files].

valid_name(S) ->
	string_util:strip_invalid_unicode(S).
