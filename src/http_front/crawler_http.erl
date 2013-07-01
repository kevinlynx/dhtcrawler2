%%
%% crawler_http.erl
%% Kevin Lynx
%% 06.15.2013
%%
-module(crawler_http).
-behaviour(gen_server).
-export([init/1, 
		 handle_info/2, 
		 handle_cast/2, 
		 handle_call/3, 
		 code_change/3, 
		 terminate/2]).
-export([start/0, 
		 start/3,
		 page_temp/0,
	     stop/0]).
-record(state, {html_temp, httpid}).

start(DBHost, DBPort, Port) ->
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/mongodb/ebin"),
	Apps = [crypto, public_key, ssl, inets, bson, mongodb],	
	[application:start(App) || App <- Apps],
	gen_server:start({local, srv_name()}, ?MODULE, [DBHost, DBPort, Port], []).

start() ->
	start(localhost, 27017, 8000).

stop() ->
	gen_server:cast(srv_name(), stop).

page_temp() ->
	gen_server:call(srv_name(), get_page_temp).

srv_name() ->
	crawler_http.

init([DBHost, DBPort, Port]) ->
	torrent_index:start_link(DBHost, DBPort),
	{ok, Pid} = inets:start(httpd, [
  	{modules, [mod_alias, mod_auth, mod_esi, mod_actions,
  		mod_cgi, mod_dir, mod_get, mod_head, mod_log, mod_disk_log]},
  	{port, Port},
    {bind_address, {0, 0, 0, 0}},
  	{server_name, "crawler_http"},
  	{document_root, "www"},
  	{server_root, "."},
    {directory_index, ["index.html"]},
  	{erl_script_alias, {"/e", [http_handler]}},
  	{mime_types, [{"html","text/html"}, 
  				  {"css","text/css"}, {"js","application/x-javascript"}]}]),
	{ok, B} = file:read_file("www/page.temp"),
	Html = binary_to_list(B),
	{ok, #state{html_temp = Html, httpid = Pid}}.

handle_call(get_page_temp, _From, State) ->
	#state{html_temp = Html} = State,
	{reply, Html, State};

handle_call(_, _From, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
	#state{httpid = Pid} = State,
	torrent_index:stop(),
	inets:stop(httpd, Pid),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(_, State) ->
    {noreply, State}.

