%%
%% sphinx_search.erl
%% Kevin Lynx
%% 07.28.2013
%%
-module(sphinx_search).
-export([init/0, search/2]).
-define(PORT, 9312).
-define(INDEX, "xml").
-define(PAGECNT, 10).

init() ->
	code:add_path("deps/giza/ebin").
	%application:start(giza).

search(Conn, Key) ->
    Q1 = giza_query:new(?INDEX, Key),
    Q2 = giza_query:port(Q1, ?PORT),
    {ok, Ret} = giza_request:send(Q2),
    decode_search_ret(Conn, Ret).

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



