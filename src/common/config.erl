%%
%% config.erl
%% Kevin Lynx
%% 07.04.2013
%%
-module(config).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1,
		 start_link/2,
		 stop/0,
		 get/1,
		 get/2]).

start_link(Name) ->
	start_link(Name, nil).

start_link(Name, Fun) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [Name, Fun], []).

stop() ->
	gen_server:cast(srv_name(), stop).

get(Key) ->
	get(Key, nil).

get(Key, Def) ->
	gen_server:call(srv_name(), {get, Key, Def}).

%% 
srv_name() ->
	?MODULE.

init([File, Fun]) ->
	FullPath = in_priv_path(File),
	State = case file:consult(FullPath) of
		{error, _Reason} ->
			Config = if Fun == nil -> []; true -> Fun() end,
			file:write_file(FullPath, io_lib:fwrite("~p.\n",[Config])),
			Config;
		{ok, [Config]} ->
			io:format("load file ~p success~n", [FullPath]),
			Config
	end,
	{ok, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call({get, Key, Def}, _From, Config) ->
	Ret = case proplists:is_defined(Key, Config) of
		false -> Def;
		true -> proplists:get_value(Key, Config)
	end,
	{reply, Ret, Config};

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

in_priv_path(Name) ->
	filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", Name]).
