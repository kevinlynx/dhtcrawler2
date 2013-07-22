%%
%% api.erl
%% Kevin Lynx
%% 07.19.2013
%% HTTP API
%%
-module(api).
-export([search/3,
		 today_top/3,
		 stats/3,
		 index/3]).
-compile(export_all).
-define(TEXT(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).
-define(MAX_FILE, 3).
-define(CONTENT_TYPE, "Content-Type: application/json\r\n\r\n").
-include("vlog.hrl").

% search?q=keyword
search(SessionID, Env, Input) ->
	Res = case http_common:get_search_keyword(Input) of
		[] -> 
			"{\"error\":\"null input\", \"suggest\":\"api/search?q=keyword\"}";
		Keyword ->
			US = http_common:list_to_utf_binary(Keyword),
			?LOG_STR(?INFO, ?FMT("API: remote ~p search /~s/", [http_common:remote_addr(Env), US])),
			do_search(Keyword)
	end,	
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Res]).

% today_top
today_top(SessionID, _Env, _Input) ->
	Rets = http_cache:today_top(),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("{\"results\":[~s]}", [BodyList]),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Body]).

% index?q=hash
index(SessionID, _Env, Input) ->
	Body = case http_common:get_view_hash(Input) of
		[] ->
			"{\"error\":\"invalid input\"}";
		Hash ->
			format_view(Hash)
	end,
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Body]).

stats(SessionID, _Env, _Input) ->
	Response = format_stats_list(http_cache:stats()),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).
%%
do_search(Keyword) ->
	{Rets, Stats} = http_cache:search(Keyword),
	{_Found, Cost, Scanned} = Stats,
	CostSecs = Cost / 1000 / 1000,
	US = http_common:list_to_utf_binary(Keyword),
	?LOG_STR(?INFO, ?FMT("API: search /~s/ found ~p, cost ~f secs", [US, Scanned, CostSecs])),
	Tip = ?TEXT("{\"keyword\":\"~s\",\"found\":~p,\"cost\":~p,", [Keyword, Scanned, Cost div 1000]),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("\"results\":[~s]}", [BodyList]),
	Tip ++ Body.
	
format_view(Hash) ->
	case db_frontend:search_one(Hash) of
		{} -> "{\"error\":\"not found\"}";
		Torrent ->
			format_torrent_detail(Torrent)
	end.

format_torrent_detail(Torrent) ->
	format_one_result(Torrent, true).

format_search_result([]) ->
	"";
format_search_result([First|Rest]) ->
	S = [format_one_result(First, false)] ++ ["," ++ format_one_result(Ret, false) || Ret <- Rest],
	lists:flatten(S).

format_one_result({single, Hash, {Name, Length}, Announce, CTime}, ShowAll) ->
	format_one_result(Hash, Name, [{Name, Length}], Announce, CTime, ShowAll);

format_one_result({multi, Hash, {Name, Files}, Announce, CTime}, ShowAll) ->	
	format_one_result(Hash, Name, Files, Announce, CTime, ShowAll).

% {"hash":"abc", "name":"abc", "created_at":time, "req":count, "file_cnt":cnt, 
% 	"files":[{"name":"abc", "size":size}]}
format_one_result(Hash, Name, Files, Announce, CTime, ShowAll) ->
	SortedFiles = http_common:sort_file_by_size(Files),
	?TEXT("{\"hash\":\"~s\",\"name\":\"~s\",\"created_at\":~p,\"req\":~p,\"file_cnt\":~p,",
		[Hash, Name, CTime, Announce, length(Files)]) ++
	"\"files\":[" ++ format_files(SortedFiles, ShowAll) ++ "]}".

format_files([], _) ->
	"";

format_files(Files, false) ->
	Max = ?MAX_FILE,
	Sub = case length(Files) >= Max of
		true ->
			lists:sublist(Files, Max);
		false ->
			Files
	end,
	do_format_files(Sub);

format_files(Files, true) ->
	do_format_files(Files).

do_format_files([First|Rest]) ->
	S = [format_file(First)] ++ ["," ++ format_file(File) || File <- Rest],
	lists:flatten(S).

format_file({more, Len}) ->	
	?TEXT("{\"name\":\"__more__\",\"size\":~b}", [Len]);

format_file({Name, Length}) ->
	?TEXT("{\"name\":\"~s\",\"size\":~b}", [Name, Length]).

format_stats_list(Stats) ->
	{TorSum, StatsList} = Stats,
	?TEXT("{\"total\":~p, \"stats\":[", [TorSum]) ++
	do_format_stats(StatsList) ++ "]}".

do_format_stats([]) ->
	"";
do_format_stats([First|Rest]) ->
	S = [format_stats(First)] ++ ["," ++ format_stats(Stats) || Stats <- Rest],
	lists:flatten(S).

format_stats({DaySec, Processed, RecvQuery, Updated, New}) ->
	?TEXT("{\"day_secs\":~p, \"recv\":~p, \"process\":~p, \"update\":~p, \"new\":~p}",
		[DaySec, RecvQuery, Processed, Updated, New]).

%%
test_search(Keyword) ->
	Filename = ?TEXT("search_~s.html", [Keyword]),
	Body = do_search(Keyword),
	file:write_file(Filename, Body).
