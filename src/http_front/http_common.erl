%%
%% http_common.erl
%% Kevin Lynx
%% 07.22.2013
%%
-module(http_common).
-export([get_search_keyword/1,
		 parse_args/1,
		 get_view_hash/1,
		 remote_addr/1,
		 list_to_utf_binary/1,
		 lookup_stats_item/2,
		 stats_to_list/1,
		 total_size/1,
		 sort_file_by_size/1]).

remote_addr(Env) ->
	proplists:get_value(remote_addr, Env).

get_search_keyword(Input) ->
	get_q_arg(Input).

get_view_hash(Input) ->
	get_q_arg(Input).

get_q_arg(Input) ->
	ReqList = parse_args(Input),
	case proplists:get_value("q", ReqList) of
		undefined -> 
			"";
		Arg ->
			Arg
	end.

parse_args(Input) ->
	D = urldecode:decode(Input),
	httpd:parse_query(D).

sort_file_by_size(Files) ->
	lists:sort(fun({_, L1}, {_, L2}) ->
		L1 > L2
	end, Files).

total_size(Files) ->
	lists:foldl(fun({_, Len}, Total) ->
		Total + Len
	end, 0, Files).

% io:format("~ts", [list_to_utf_binary(L)])
% io:format(FP, "~s", [list_to_utf_binary(L)])
list_to_utf_binary(L) ->	
	BK = list_to_binary(L),
	UL = unicode:characters_to_list(BK),
	US = unicode:characters_to_binary(UL),
	US.

lookup_stats_item(Stats, Field) ->
	case bson:lookup(Field, Stats) of
		{} -> 0;
		{Val} -> Val
	end.

stats_to_list(Stats) ->
	Fileds = ['_id', get_peers_query, get_peers, updated, new_saved, 
		inserted_query, filter_hash],
	[lookup_stats_item(Stats, F) || F <- Fileds].
