%%
%% http_common.erl
%% Kevin Lynx
%% 07.22.2013
%%
-module(http_common).
-export([get_search_keyword/1,
		 get_view_hash/1,
		 sort_file_by_size/1]).

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
