%%
%% index_download.erl
%% Kevin Lynx
%% 07.14.2013
%%
-module(index_download).
-include("vlog.hrl").
-export([download/0, download/2, do_download/2]).
-define(DOMAIN, "http://torrage.com").

download() ->
 	{Date, _} = calendar:local_time(),
	download(self(), Date).

download(From, Date) ->
	spawn_link(?MODULE, do_download, [From, Date]).

do_download(From, {_, _, _} = Date) ->
	File = format_file_name(Date),
	URL = format_file_url(?DOMAIN, File),
	io:format("download file ~s~n", [URL]),
	Start = now(),
	case ibrowse:send_req(URL, [], get, [], [], infinity) of
		{ok, Code, _, Body} ->
			Ret = check_result(File, Code, Body, timer:now_diff(now(), Start) div 1000),
			From ! Ret;
		{error, Reason} ->
			?E(?FMT("download sync file ~s failed ~p", [URL, Reason])),
			From ! {sync_torrent_index, failed, File}
	end.

check_result(File, "200", Body, Time) ->
	Size = length(Body),
	Speed = Size * 1000 div Time,
	io:format("download index file ~s success ~b bytes, ~b bytes/sec~n", [File, Size, Speed]),
	{sync_torrent_index, ok, File, Body};

check_result(File, Code, _, _) ->
	io:format("download index file ~s failed ~p~n", [File, Code]),
	{sync_torrent_index, failed, File}.

format_file_name({Y, M, 0}) ->
 	lists:flatten(io_lib:format("~b~2..0b.txt", [Y, M]));
format_file_name({Y, M, D}) ->
 	lists:flatten(io_lib:format("~b~2..0b~2..0b.txt", [Y, M, D])).

format_file_url(Domain, File) ->
	Domain ++ "/sync/" ++ File.
