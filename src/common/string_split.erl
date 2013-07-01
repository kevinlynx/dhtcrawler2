%%
%% string_split.erl
%% Kevin Lynx
%% 06.17.2013
%% split a string into substrings
%%
-module(string_split).
-export([split/1]).
-compile(export_all).

split(Str) when is_list(Str) ->
	B = list_to_binary(Str),
	case unicode:characters_to_list(B) of
		{error, L, D} ->
			{error, L, D};
		{incomplete, L, D} ->
			{incomplete, L, D};
		UL ->
		{ok, subsplit(UL)}
	end.

subsplit([]) ->
	[];

subsplit(L) ->
	[_|R] = L,
	{PreL, _} = lists:splitwith(fun(Ch) -> not is_spliter(Ch) end, L),
	[unicode:characters_to_binary(lists:sublist(PreL, Len)) 
		|| Len <- lists:seq(1, length(PreL))] ++ subsplit(R).

% TODO: more control characters
is_spliter(Ch) ->
	(Ch < $0) or
	((Ch >= $[) and (Ch =< $`)) or
	((Ch >= ${) and (Ch =< $~)) or
	((Ch >= $:) and (Ch =< $@)).




