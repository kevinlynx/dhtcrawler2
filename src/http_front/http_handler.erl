%%
%% http_handler.erl
%% Kevin Lynx
%% 06.18.2013
%%
-module(http_handler).
-export([search/3,
		 test_search/1,
		 index/3,
		 stats/3,
		 recent/3,
		 today_top/3,
		 top/3]).
-define(TEXT(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).
-import(torrent_file, [size_string/1]).
-define(CONTENT_TYPE, "Content-Type: text/html\r\n\r\n").

search(SessionID, _Env, Input) ->
	{K, Body} = case get_search_keyword(Input) of
		[] -> 
			{"", "invalid input"};
		Key ->
			{Key, do_search(Key)}
	end,
	Response = simple_html(K, Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

top(SessionID, _Env, _Input) ->
	Rets = db_frontend:all_top(),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("<ol>~s</ol>", [lists:flatten(BodyList)]),
	Response = simple_html("top", Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

today_top(SessionID, _Env, _Input) ->
	Rets = db_frontend:today_top(),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("<ol>~s</ol>", [lists:flatten(BodyList)]),
	Response = simple_html("today_top", Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

recent(SessionID, _Env, _Input) ->
	Rets = db_frontend:newest(),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("<ol>~s</ol>", [lists:flatten(BodyList)]),
	Response = simple_html("recent", Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

stats(SessionID, _Env, _Input) ->
	{TorSum, StatsList} = db_frontend:stats(),
	Body = ?TEXT("<h3>total ~p torrents</h3>", [TorSum]) ++
		"<ul>" ++ 
		format_stats(StatsList) ++
		"</ul>",
	Response = simple_html("", Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

index(SessionID, _Env, Input) ->
	Body = case get_index_hash(Input) of
		[] ->
			"invalid hash";
		Hash ->
			format_view(Hash)
	end,
	Response = simple_html("", Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

get_search_keyword(Input) ->
	case string:equal(string:substr(Input, 1, 2), "q=") of
		true ->
			urldecode:decode(string:substr(Input, 3));
		false ->
			[]
	end.

get_index_hash(Input) ->
	case string:equal(string:substr(Input, 1, 2), "q=") of
		true ->
			string:substr(Input, 3);
		false ->
			[]
	end.

simple_html(Key, Body) ->
 	?TEXT(crawler_http:page_temp(), [Key, Body]).
	
test_search(Keyword) ->
	Filename = ?TEXT("search_~s.html", [Keyword]),
	Body = do_search(Keyword),
	file:write_file(Filename, simple_html(Keyword, Body)).

do_search(Keyword) ->
	{Rets, Stats} = db_frontend:search(Keyword),
	{_Found, Cost, Scanned} = Stats,
	Tip = ?TEXT("<h4>search ~s, ~b results, ~f seconds</h4>", 
		[Keyword, Scanned, Cost / 1000 / 1000]),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("<ol>~s</ol>", [lists:flatten(BodyList)]),
	Tip ++ Body.
	
format_search_result(RetList) ->
	[format_one_result(Result, false) || Result <- RetList].

format_one_result({single, Hash, {Name, Length}, Announce, CTime}, ShowAll) ->
	format_one_result(Hash, Name, [{Name, Length}], Announce, CTime, ShowAll);

format_one_result({multi, Hash, {Name, Files}, Announce, CTime}, ShowAll) ->	
	format_one_result(Hash, Name, Files, Announce, CTime, ShowAll).

format_one_result(Hash, Name, Files, Announce, CTime, ShowAll) ->
	?TEXT("<li><p style=\"font-size: 120%;\">
		<a target='_blank' href=\"/e/http_handler:index?q=~s\">~s</a></p><ul>~s</ul>",
		[Hash, Name, format_files(Files, ShowAll)]) ++
	?TEXT("<p style=\"font-size:80%\">Index at: ~s  |  File count: ~p  |  Announce count: ~p
		<a href=\"~s\" style=\"font-size:120%\">  Download</a></p>",
		[format_time_string(CTime), length(Files), Announce, format_magnet(Hash)]).

format_files(Files, false) ->
	Sub = case length(Files) > 3 of
		true ->
			lists:sublist(Files, 3) ++ [{more, length(Files) - 3}];
		false ->
			Files
	end,
	lists:flatten([format_file(File) || File <- Sub]);

format_files(Files, true) ->
	lists:flatten([format_file(File) || File <- Files]).

format_file({more, Len}) ->	
	?TEXT("<li>...~b more files</li>", [Len]);

format_file({Name, Length}) ->
	?TEXT("<li>~s <span style=\"color:#888;\">~s</span></li>", 
		[Name, size_string(Length)]).

format_view(Hash) ->
	case db_frontend:search_one(Hash) of
		{} -> "not found";
		Torrent ->
			format_torrent_detail(Torrent)
	end.

format_torrent_detail(Torrent) ->
	format_one_result(Torrent, true).

format_magnet(MagHash) ->
	"magnet:?xt=urn:btih:" ++ MagHash.

format_stats([]) ->
	[];

format_stats([{DaySec, Processed, RecvQuery, Updated, New}|Rest]) ->
	?TEXT("<li>~s RecvQuery ~p ProcessedQuery ~p Updated ~p New ~p</li>", 
		[format_date_string(DaySec), RecvQuery, Processed, Updated, New]) ++ 
	format_stats(Rest).

format_time_string(Secs) ->
	{{Y, M, D}, {H, Min, Sec}} = time_util:seconds_to_local_time(Secs),
	?TEXT("~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", 
		[Y, M, D, H, Min, Sec]).

format_date_string(Secs) ->
	{{Y, M, D}, _} = time_util:seconds_to_local_time(Secs),
	?TEXT("~b-~2..0b-~2..0b", [Y, M, D]).

