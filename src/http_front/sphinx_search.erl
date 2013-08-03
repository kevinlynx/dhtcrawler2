%%
%% sphinx_search.erl
%% Kevin Lynx
%% 07.28.2013
%%
-module(sphinx_search).
-include("vlog.hrl").
-export([search/4]).
-define(PORT, 9312).
-define(INDEX, "xml").

search(Conn, Key, Offset, Count) ->
    Q1 = giza_query:new(?INDEX, Key),
    Q2 = giza_query:port(Q1, ?PORT),
    Q3 = giza_query:offset(Q2, Offset),
    Q4 = giza_query:limit(Q3, Count),
    T1 = now(),
    {T2, TDocs} = case catch giza_request:send(Q4) of
    	{'EXIT', R} ->
    		?W(?FMT("sphinx search error ~p", [R])),
    		{now(), []};
    	{ok, Ret} ->
    		{now(), decode_search_ret(Conn, Ret)}
    end,
    T3 = now(),
    Stats = {timer:now_diff(T2, T1), timer:now_diff(T3, T2)},
    {TDocs, Stats}.

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



