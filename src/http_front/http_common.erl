%%
%% http_common.erl
%% Kevin Lynx
%% 07.22.2013
%%
-module(http_common).
-export([get_search_keyword/1,
		 get_view_hash/1,
		 remote_addr/1,
		 list_to_utf_binary/1,
		 sort_file_by_size/1]).

remote_addr(Env) ->
	proplists:get_value(remote_addr, Env).

get_search_keyword(Input) ->
	get_q_arg(Input).

get_view_hash(Input) ->
	get_q_arg(Input).

get_q_arg(Input) ->
	D = urldecode:decode(Input),
	ReqList = httpd:parse_query(D),
	case proplists:get_value("q", ReqList) of
		undefined -> 
			"";
		Arg ->
			Arg
	end.

sort_file_by_size(Files) ->
	lists:sort(fun({_, L1}, {_, L2}) ->
		L1 > L2
	end, Files).

% io:format("~ts", [list_to_utf_binary(L)])
% io:format(FP, "~s", [list_to_utf_binary(L)])
list_to_utf_binary(L) ->	
	BK = list_to_binary(L),
	UL = unicode:characters_to_list(BK),
	US = unicode:characters_to_binary(UL),
	US.


