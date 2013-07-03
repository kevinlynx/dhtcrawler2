%%
%% loc_torrent.erl
%% Kevin Lynx
%% 07.03.2013
%%
-module(loc_torrent).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1,
		 stop/1]).
-record(state, {downcount = 0, dbpool, downloader}).
% when the wait_download is empty,
-define(WAIT_TIME, 2*60*1000).
-define(INTERVAL, 100).
-define(MAX_DOWNLOAD, 50).

start_link(DBPool) ->
	gen_server:start_link(?MODULE, [DBPool], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

%%
init([DBPool]) ->
	{ok, DownPid} = tor_download:start_link(),
	{ok, #state{dbpool = DBPool, downloader = DownPid}, 0}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info({got_torrent, failed, _Hash}, State) ->
	#state{downcount = C} = State,
	{_, NewState} = download_next(State),
	{noreply, NewState#state{downcount = C - 1}};

handle_info({got_torrent, ok, Hash, Content}, State) ->
	#state{downcount = C} = State,
	Conn = db_conn(State),
	db_loc_torrent:save(Conn, Hash, Content),
	{_, NewState} = download_next(State),
	{noreply, NewState#state{downcount = C - 1}};

handle_info(timeout, State) ->
	{Status, NewState} = download_next(State),
	if Status == ok -> schedule_best(); true -> ok end,
	{noreply, NewState};

handle_info(_, State) ->
	{noreply, State}.

% to avoid the high cpu/network at startup
schedule_best() ->
	timer:send_after(?INTERVAL, timeout).		

download_next(#state{downcount = Count} = State) when Count < ?MAX_DOWNLOAD ->
	#state{downloader = DownPid} = State,
	Conn = db_conn(State),
	{Status, NewCount} = case db_loc_torrent:load_hash(Conn) of
		[] ->
			wait_download(),
			{wait, Count};
		Hash ->
			tor_download:download(DownPid, Hash),
			{ok, Count + 1}
	end,
	{Status, State#state{downcount = NewCount}};

download_next(State) ->
	{wait, State}.

wait_download() ->
	timer:send_after(?WAIT_TIME, timeout).

db_conn(State) ->
	#state{dbpool = DBPool} = State,
	mongo_pool:get(DBPool).
