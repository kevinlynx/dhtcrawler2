%%
%% index_download.erl
%% Kevin Lynx
%% 07.14.2013
%%
-module(index_download).
-export([download/0, download/2, today_file_name/0, do_download/2]).
-define(DOMAIN, "http://torrage.com").
-define(WORKDIR, "sync/").

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
	{ok, Code, _, Body} = ibrowse:send_req(URL, [], get, [], [], infinity),
	Dir = ?WORKDIR,
	filelib:ensure_dir(Dir),
	FullFile = Dir ++ File,
	Ret = try_save(FullFile, Code, Body, timer:now_diff(now(), Start) div 1000),
	From ! Ret.

try_save(FullFile, "200", Body, Time) ->
	file:write_file(FullFile, Body),
	Size = length(Body),
	Speed = Size * 1000 div Time,
	io:format("download index file ~s success ~b bytes, ~b bytes/sec~n", [FullFile, Size, Speed]),
	{sync_torrent_index, ok, FullFile};
try_save(FullFile, Code, _, _) ->
	io:format("download index file ~s failed ~p~n", [FullFile, Code]),
	{sync_torrent_index, failed, FullFile}.

today_file_name() ->
 	{Date, _} = calendar:local_time(),
 	?WORKDIR ++ format_file_name(Date).

format_file_name({Y, M, 0}) ->
 	lists:flatten(io_lib:format("~b~2..0b.txt", [Y, M]));
format_file_name({Y, M, D}) ->
 	lists:flatten(io_lib:format("~b~2..0b~2..0b.txt", [Y, M, D])).

format_file_url(Domain, File) ->
	Domain ++ "/sync/" ++ File.


