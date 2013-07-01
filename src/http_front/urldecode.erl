%% 
%% urldecode.erl
%%
-module(urldecode).
-export([decode/1]).
-compile(export_all).

decode([$%, Hi, Lo | Tail]) ->
	Hex = unhexdigit(Hi, Lo),
	[Hex | decode(Tail)];
decode([$?|T]) ->
	[$?|T];
decode([H|T]) when is_integer(H) ->
	[H |decode(T)];
decode([]) ->
	[];
decode([H|T]) when is_list(H) ->
	[decode(H) | decode(T)].

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.

unhexdigit(HiCh, LoCh) ->
	unhexdigit(LoCh) bor (unhexdigit(HiCh) bsl 4).

test(S) ->
	R = decode(S),
	file:write_file("decode.txt", R).
