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
		 real_stats/3,
		 recent/3,
		 today_top/3,
		 append_page_nav/3,
		 top/3]).
-define(TEXT(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).
-import(torrent_file, [size_string/1]).
-define(CONTENT_TYPE, "Content-Type: text/html\r\n\r\n").
-define(COUNT_PER_PAGE, 10).
-define(PAGE_NAV_MAX, 10).
-include("vlog.hrl").

search(SessionID, Env, Input) ->
	{K, Body} = case http_common:get_search_keyword(Input) of
		[] -> 
			{"", "invalid input"};
		Key ->
			US = http_common:list_to_utf_binary(Key),
			?LOG_STR(?INFO, ?FMT("remote ~p search /~s/", [http_common:remote_addr(Env), US])),
			Args = http_common:parse_args(Input),
			Page = case proplists:get_value("p", Args) of undefined -> 0; Val -> list_to_integer(Val) end,
			{Key, do_search(Key, Page)}
	end,
	Response = simple_html(K, Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

top(SessionID, _Env, _Input) ->
	Rets = db_frontend:all_top(),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("<ol>~s</ol>", [lists:flatten(BodyList)]),
	Response = simple_html("", Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

today_top(SessionID, _Env, _Input) ->
	Rets = http_cache:today_top(),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("<ol>~s</ol>", [lists:flatten(BodyList)]),
	Response = simple_html("", Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

recent(SessionID, _Env, _Input) ->
	Rets = db_frontend:newest(),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("<ol>~s</ol>", [lists:flatten(BodyList)]),
	Response = simple_html("recent", Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

stats(SessionID, _Env, _Input) ->
	Response = format_stats_list(http_cache:stats()),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

real_stats(SessionID, _Env, _Input) ->
	Response = format_stats_list(db_frontend:stats()),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

format_stats_list(Stats) ->
	{TorSum, StatsList} = Stats,
	Body = ?TEXT("<h3>total ~p torrents</h3>", [TorSum]) ++
		"<ul>" ++ 
		format_stats(StatsList) ++
		"</ul>",
	simple_html("", Body).

index(SessionID, _Env, Input) ->
	Body = case http_common:get_view_hash(Input) of
		[] ->
			"invalid hash";
		Hash ->
			format_view(Hash)
	end,
	Response = simple_html("", Body),
	mod_esi:deliver(SessionID, [?CONTENT_TYPE, Response]).

simple_html(Key, Body) ->
 	?TEXT(crawler_http:page_temp(), [Key, Body]).
	
test_search(Keyword) ->
	Filename = ?TEXT("search_~s.html", [Keyword]),
	Body = do_search(Keyword, 0),
	file:write_file(Filename, simple_html(Keyword, Body)).

do_search(Keyword, Page) ->
	case config:get(search_method, mongodb) of
		mongodb ->
			search_by_mongo(Keyword);
		sphinx ->
			search_by_sphinx(Keyword, Page)
	end.

search_by_mongo(Keyword) ->
	{Rets, Stats} = http_cache:search(Keyword),
	{_Found, Cost, Scanned} = Stats,
	CostSecs = Cost / 1000 / 1000,
	US = http_common:list_to_utf_binary(Keyword),
	?LOG_STR(?INFO, ?FMT("search /~s/ found ~p, cost ~f secs", [US, Scanned, CostSecs])),
	Tip = ?TEXT("<h4>search ~s, ~b results, ~f seconds</h4>", 
		[Keyword, Scanned, CostSecs ]),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("<ol>~s</ol>", [lists:flatten(BodyList)]),
	Tip ++ Body.
	
search_by_sphinx(Keyword, Page) ->
	{Rets, Stats} = db_frontend:search(Keyword, Page, ?COUNT_PER_PAGE),
	{TotalFound, CostTime, DBTime} = Stats,
	US = http_common:list_to_utf_binary(Keyword),
	?LOG_STR(?INFO, ?FMT("search /~s/ found ~p, cost ~b sp ms, ~b db ms", 
		[US, TotalFound, CostTime, DBTime / 1000])),
	Tip = ?TEXT("<h4>search ~s, ~b results, ~f seconds, db ~f seconds</h4>", 
		[Keyword, TotalFound, CostTime / 1000, DBTime / 1000 / 1000]),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("<ol>~s</ol>", [lists:flatten(BodyList)]),
	Tip ++ Body ++ append_page_nav(Keyword, Page, TotalFound).

append_page_nav(Key, ThisPage, Total) ->
	TotalPage = Total div ?COUNT_PER_PAGE,
	StartPage = case ThisPage - ?PAGE_NAV_MAX div 3 of
		N when N < 0 -> 0;
		N -> N
	end,
	EndPage = case StartPage + ?PAGE_NAV_MAX of
		E when E > TotalPage -> TotalPage;
		E -> E
	end,
	Links = lists:foldl(fun(I, Str) ->
		D = I + 1,
		Str ++ if I == ThisPage ->
			?TEXT("&nbsp;~p&nbsp;", [D]);
			true ->format_page_nav(Key, I, integer_to_list(D))
		end
	end, [], lists:seq(StartPage, EndPage)),
	FirstTip = if StartPage > 0 -> format_page_nav(Key, 0, "1") ++ "..."; true -> [] end,
	"<p class=\"page-nav\">" ++ FirstTip ++ Links ++ "</p>".

format_page_nav(Key, Page, Tip) ->
	?TEXT("&nbsp;<a href=\"http_handler:search?q=~s&p=~p\">~s</a>&nbsp;", [Key, Page, Tip]).

format_search_result(RetList) ->
	[format_one_result(Result, false) || Result <- RetList].

format_one_result({single, Hash, {Name, Length}, Announce, CTime}, ShowAll) ->
	format_one_result(Hash, Name, [{Name, Length}], Announce, CTime, ShowAll);

format_one_result({multi, Hash, {Name, Files}, Announce, CTime}, ShowAll) ->	
	format_one_result(Hash, Name, Files, Announce, CTime, ShowAll).

format_one_result(Hash, Name, Files, Announce, CTime, ShowAll) ->
	SortedFiles = http_common:sort_file_by_size(Files),
	?TEXT("<li><p class=\"search-title\">
		<a target='_blank' href=\"/e/http_handler:index?q=~s\">~s</a></p><ul>~s</ul>",
		[Hash, Name, format_files(SortedFiles, ShowAll)]) ++
	?TEXT("<p class=\"search-detail\">Index at: ~s | File count: ~p | Query count: ~p | Total Size: ~s
		<a href=\"~s\" class=\"download-tip\">  Download</a></p>",
		[format_time_string(CTime), length(Files), Announce, size_string(http_common:total_size(Files)), format_magnet(Hash)]).

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
	?TEXT("<li>~s <span class=\"file-size\">~s</span></li>", 
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

format_stats([Stats|Rest]) ->
	[DaySec|Vals] = http_common:stats_to_list(Stats),
	?TEXT("<li>~s RecvQ ~p ProcessQ ~p Updated ~p <b>New ~p</b> UniqueQ ~p Filtered ~p</li>",
		[format_date_string(DaySec)|Vals]) ++
	format_stats(Rest).

format_time_string(Secs) ->
	{{Y, M, D}, {H, Min, Sec}} = time_util:seconds_to_local_time(Secs),
	?TEXT("~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", 
		[Y, M, D, H, Min, Sec]).

format_date_string(Secs) ->
	{{Y, M, D}, _} = time_util:seconds_to_local_time(Secs),
	?TEXT("~b-~2..0b-~2..0b", [Y, M, D]).
