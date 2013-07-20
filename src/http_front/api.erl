%%
%% api.erl
%% Kevin Lynx
%% 07.19.2013
%% HTTP API
%%
-module(api).
-export([search/3]).
-compile(export_all).
-define(TEXT(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).
-define(MAX_FILE, 10).

search(SessionID, _Env, Input) ->
	Res = case crawler_http:get_search_keyword(Input) of
		[] -> 
			"{\"error\":\"null input\", \"suggest\":\"api/search?q=keyword\"}";
		Keyword ->
			do_search(Keyword)
	end,	
	mod_esi:deliver(SessionID, ["Content-Type: application/json\r\n\r\n", Res]).

do_search(Keyword) ->
	{Rets, Stats} = http_cache:search(Keyword),
	{_Found, Cost, Scanned} = Stats,
	Tip = ?TEXT("{\"keyword\":\"~s\", \"found\":~p, \"cost\":~p,", [Keyword, Scanned, Cost div 1000]),
	BodyList = format_search_result(Rets),
	Body = ?TEXT("\"results\":[~s]}", [BodyList]),
	Tip ++ Body.
	
format_search_result([]) ->
	"";
format_search_result([First|Rest]) ->
	S = [format_one_result(First, false)] ++ ["," ++ format_one_result(Ret, false) || Ret <- Rest],
	lists:flatten(S).

format_one_result({single, Hash, {Name, Length}, Announce, CTime}, ShowAll) ->
	format_one_result(Hash, Name, [{Name, Length}], Announce, CTime, ShowAll);

format_one_result({multi, Hash, {Name, Files}, Announce, CTime}, ShowAll) ->	
	format_one_result(Hash, Name, Files, Announce, CTime, ShowAll).

format_one_result(Hash, Name, Files, Announce, CTime, ShowAll) ->
	?TEXT("{\"hash\":\"~s\", \"name\":\"~s\", \"created_at\":~p, \"req\":~p,",
		[Hash, Name, CTime, Announce]) ++
	"\"files\":[" ++ format_files(Files, ShowAll) ++ "]}".

format_files([], _) ->
	"";

format_files(Files, false) ->
	Max = ?MAX_FILE,
	Sub = case length(Files) >= Max of
		true ->
			lists:sublist(Files, Max) ++ [{more, length(Files) - Max}];
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

%%
test_search(Keyword) ->
	Filename = ?TEXT("search_~s.html", [Keyword]),
	Body = do_search(Keyword),
	file:write_file(Filename, Body).
