%%
%% sphinx_xml.erl
%% Kevin Lynx
%% Write sphinx xml file
%%
-module(sphinx_xml).
-behaviour(gen_server).
-include("vlog.hrl").
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/0, insert/1, force_save/0]).
-record(state, {docs = [], max, startid = -1}).

start_link() ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

insert(Doc) ->
	gen_server:cast(srv_name(), {insert, Doc}).

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

handle_cast(save, #state{docs = Docs} = State) when length(Docs) > 0 ->
	#state{startid = StartID} = State,
	EndID = length(Docs) + StartID - 1,
	try_save(Docs, 0, StartID, EndID),
	{noreply, State#state{docs = []}};

handle_cast({insert, {Hash, Name, Files, ID, Query, CreatedAt}}, State) ->
	#state{docs = Docs, max = Max, startid = StartID} = State,
	NewStartID = if length(Docs) == 0 -> ID; true -> StartID end,
	Doc = sphinx_doc:element(Hash, Name, Files, ID, Query, CreatedAt),
	NewDocs = try_save([Doc|Docs], Max, NewStartID, ID),
	{noreply, State#state{docs = NewDocs, startid = NewStartID}};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

try_save(Docs, Max, StartID, NowID) when length(Docs) >= Max, length(Docs) > 0 ->
	File = config:get(delta_source_file),
	Conf = config:get(sphinx_config_file),
	Delta = config:get(delta_index_name),
	Main = config:get(main_index_name),
	io:format("sync sphinx index ~p documents...", [length(Docs)]),
	?I(?FMT("save sphinx xml file ~s", [File])),
	sphinx_doc:write_xml(File, Docs),
	?I(?FMT("build delta index : ~s", [Delta])),
	sphinx_cmd:build_delta_index(File, Delta, Conf, StartID, NowID),
	?I(?FMT("merge delta to main index ~s -> ~s", [Delta, Main])),
	sphinx_cmd:merge_index(Main, Delta, Conf),
	?I("index updated done"),
	io:format("done~n", []),
	[];
try_save(Docs, _, _, _) ->
	Docs.
