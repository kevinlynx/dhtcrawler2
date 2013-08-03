%%
%% sphinx_search.erl
%% Kevin Lynx
%% 07.28.2013
%%
-module(sphinx_search).
-include("vlog.hrl").
-export([search/4, search_hash/3]).
-define(PORT, 9312).
-define(INDEX, "xml").

search(Conn, Key, Offset, Count) ->
    Rets = search_hash(Key, Offset, Count),
    TotalFound = proplists:get_value(total_found, Rets),
    CostTime = proplists:get_value(time, Rets),
    Hashes = proplists:get_value(match, Rets),
    T1 = now(),
    Tors = decode_search_ret(Conn, Hashes),
    DBUsed = timer:now_diff(now(), T1),
    Stats = {TotalFound, CostTime, DBUsed},
    {Tors, Stats}.

search_hash(Key, Offset, Count) ->
    Q1 = giza_query:new(?INDEX, Key),
    Q2 = giza_query:port(Q1, ?PORT),
    Q3 = giza_query:offset(Q2, Offset),
    Q4 = giza_query:limit(Q3, Count),
    case catch giza_request:send(Q4) of
        {'EXIT', R} ->
            ?W(?FMT("sphinx search error ~p", [R])),
            failed;
        {ok, Ret} ->
            Ret
    end.

decode_search_ret(Conn, Ret) ->
	Hashes = [translate_hash(Item) || Item <- Ret],
	[db_store_mongo:index(Conn, Hash) || Hash <- Hashes].

translate_hash({_DocID, Item}) ->
	Attrs = proplists:get_value(attrs, Item),
	H1 = proplists:get_value(<<"hash1">>, Attrs),
	H2 = proplists:get_value(<<"hash2">>, Attrs),
	H3 = proplists:get_value(<<"hash3">>, Attrs),
	H4 = proplists:get_value(<<"hash4">>, Attrs),
	H5 = proplists:get_value(<<"hash5">>, Attrs),
	Hash = sphinx_id:tohash({H1, H2, H3, H4, H5}),
	40 = length(Hash),
	Hash.



