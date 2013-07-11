%%
%% tor_download.erl
%% Kevin Lynx
%% 06.30.2013
%%
-module(tor_download).
-include("vlog.hrl").
-behaviour(gen_server).
-export([init/1, 
		 handle_info/2, 
		 handle_cast/2, 
		 handle_call/3, 
		 code_change/3, 
		 terminate/2]).
-export([start_global/0,
		 start_link/0,
		 stop/1,
		 download/2,
		 stats/1]).
-export([test/2]).
-define(HTTP_SESSION, 5000).
-define(HTTP_PIPELINE, 1000).
-define(REQ_TIMEOUT, 60*1000).
% when ibrowse crashed, it will not notify these requests timeout, that will
% make these requests stay in the state forever
-define(REQ_ERROR_TIMEOUT, 2*?REQ_TIMEOUT).
-define(IS_ERROR_TIMEOUT(T), (timer:now_diff(now(), T) div 1000 > ?REQ_ERROR_TIMEOUT)).
-record(state, {start, hashSum = 0, reqSum = 0, totalTime = 0, reqs}).

start_global() ->
	ibrowse:start(),
	Options = [{max_sessions, ?HTTP_SESSION}, {max_pipeline_size, ?HTTP_PIPELINE}],
	% not work here ?
	[ibrowse:set_dest(Host, 80, Options) || Host <- get_req_hosts()],
	ok.

start_link() ->
	gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

download(Pid, MagHash) when is_list(MagHash), length(MagHash) == 40 ->
	gen_server:cast(Pid, {download, MagHash, self()}).

%{HashSum, ReqSum, TotalTime, CurrentReqCount}
stats(Pid) ->
	gen_server:call(Pid, get_stats, infinity).

init([]) ->
	{ok, #state{start = now(), reqs = gb_trees:empty()}}.

handle_cast({download, MagHash, From}, State) ->
	#state{reqs = Reqs, hashSum = H, reqSum = R} = State,
	% remove these invalid requests
	UpdateReqs = Reqs, %check_error_timeout_reqs(Reqs),
	NewReqs = create_download(UpdateReqs, MagHash, From),
	NewSum = R + 1 - gb_trees:size(Reqs) - gb_trees:size(UpdateReqs),
	{noreply, State#state{reqs = NewReqs, hashSum = H + 1, reqSum = NewSum}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_response(State, ReqID, Body) ->
	#state{reqSum = R, totalTime = T, reqs = Reqs} = State,
	{MagHash, URLs, From, ThisStart, Start} = gb_trees:get(ReqID, Reqs),
	NewT = T + timer:now_diff(now(), ThisStart) div 1000, % mill-seconds
	{NewReqS, NewReqs} = case unzip_content(Body) of
		error ->
			handle_next_req(MagHash, URLs, From, R, Start, ReqID, Reqs);
		Content ->
			{R, handle_ok_response(MagHash, Content, From, ReqID, Reqs)}
	end,
	State#state{reqSum = NewReqS, reqs = NewReqs, totalTime = NewT}.

handle_info({ibrowse_async_response, ReqID, Body}, State) ->
	#state{reqs = Reqs} = State,
	NewState = case gb_trees:is_defined(ReqID, Reqs) of
		true ->
			handle_response(State, ReqID, Body);
		false ->
			?E(?FMT("not found req ~p , reqs count ~p", [ReqID, gb_trees:size(Reqs)])),
			State
	end,
	{noreply, NewState};

handle_info(check_error_timeout, State) ->
	#state{reqs = Reqs} = State,
	NewReqs = check_error_timeout_reqs(Reqs),
	{noreply, State#state{reqs = NewReqs}};
	
handle_info(_, State) ->
    {noreply, State}.

handle_call(get_stats, _From, State) ->
	#state{hashSum = H, reqSum = R, totalTime = T, reqs = Reqs} = State,
	{reply, {H, R, T, gb_trees:size(Reqs)}, State};

handle_call(_, _From, State) ->
	{noreply, State}.
%%
handle_next_req(MagHash, URLs, From, ReqSum, Start, ReqID, Reqs) ->
	DelReqs = gb_trees:delete(ReqID, Reqs),
	case request_next(URLs) of
		{error, empty} ->
			From ! {got_torrent, failed, MagHash},
			{ReqSum, DelReqs};
		{ok, NewReqID, NewURLs, Time} ->
			% REQUEST, record the original download request start time
			NewReq = {MagHash, NewURLs, From, Time, Start},
			{ReqSum + 1, gb_trees:insert(NewReqID, NewReq, DelReqs)}
	end.

handle_ok_response(MagHash, Content, From, ReqID, Reqs) ->
	From ! {got_torrent, ok, MagHash, Content},
	gb_trees:delete(ReqID, Reqs).

create_download(Reqs, MagHash, From) ->
	URLs = create_req_urls(MagHash),
	case request_next(URLs) of 
		{ok, ReqID, NewURLs, Time} ->
			% REQUEST
			Req = {MagHash, NewURLs, From, Time, Time},
			gb_trees:insert(ReqID, Req, Reqs);
		{error, empty} -> % exception
			From ! {got_torrent, failed, MagHash},
			Reqs
	end.

request_next([]) ->
	{error, empty};

request_next([URL|T]) ->
	SSL = is_ssl_url(URL),
	Options = [{is_ssl, SSL}, {ssl_options, []}, {stream_to, self()},
		{max_sessions, ?HTTP_SESSION}, {max_pipeline_size, ?HTTP_PIPELINE}],
	case ibrowse:send_req(URL, [], get, [], Options, ?REQ_TIMEOUT) of
		{ibrowse_req_id, ReqID} ->
			{ok, ReqID, T, now()};
		Reason ->
			?E(?FMT("ibrowse send_req failed ~p", [Reason])),
			request_next(T)
	end.
%%
unzip_content(B) when is_list(B) ->
	unzip_content(list_to_binary(B));

unzip_content(B) when is_binary(B), byte_size(B) > 0 ->
	case catch(zlib:gunzip(B)) of
		{'EXIT', _} -> error;
		Res -> Res
	end;

unzip_content(_B) ->
	error.

%% http stuff
get_req_hosts() ->
	["http://bt.box.n0808.com", 
   	"http://torcache.net",
   	"http:/torrange.com",
   	"http://zoink.it"].

create_req_urls(MagHash) when is_list(MagHash), length(MagHash) == 40 ->
	U1 = "http://torcache.net/torrent/" ++ MagHash ++ ".torrent",
	U2 = format_btbox_url(MagHash),
	U3 = "http://torrage.com/torrent/" ++ MagHash ++ ".torrent",
	% zoink.it support https, but the ssl library seems memory leak
	U4 = "http://zoink.it/torrent/" ++ MagHash ++ ".torrent",
	[U1, U2, U3, U4].	

is_ssl_url(URL) when is_list(URL), length(URL) > 4 ->
	string:substr(URL, 1, 5) == "https".

format_btbox_url(MagHash) ->
	H = lists:sublist(MagHash, 2),
	T = lists:nthtail(38, MagHash),
	"http://bt.box.n0808.com/" ++ H ++ "/" ++ T ++ "/" ++ MagHash ++ ".torrent".

check_error_timeout_reqs(Reqs) ->
	ReqList = gb_trees:to_list(Reqs),
	lists:foldl(fun(E, NewReqs) ->
		check_error_timeout(NewReqs, E)
	end, gb_trees:empty(), ReqList).

check_error_timeout(Acc, {ReqID, {MagHash, _, From, _, Start} = Req}) ->
	case ?IS_ERROR_TIMEOUT(Start) of
		true ->
			From ! {got_torrent, failed, MagHash},
			?E(?FMT("download req error timeout ~s", [MagHash])),
			Acc;
		false ->
			gb_trees:insert(ReqID, Req, Acc)
	end.
%%
test(Pid, MagHash) ->
	tor_download:download(Pid, MagHash),
	filelib:ensure_dir("torrents/"),
	Name = "torrents/" ++ MagHash ++ ".torrent",
	receive 
		{got_torrent, ok, _, Content} ->
			file:write_file(Name, Content);
		{got_torrent, failed, _} ->
			failed
	after 8000 ->
		timeout
	end.

