%%
%% sphinx_search.erl
%% Kevin Lynx
%% 07.28.2013
%%
-module(sphinx_search).
-include("vlog.hrl").
-export([search/4, search_hash/3, highlight_title/2, highlight_files/2]).
-define(PORT, 9312).
-define(INDEX, "xml").
-compile(export_all).

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

highlight_title(Key, Name) when is_list(Name) ->
    R = case catch sphinx_excerpt:build_excerpt(list_to_binary(Key), [list_to_binary(Name)], list_to_binary(?INDEX)) of
        {'EXIT', Reason} ->
            ?E(?FMT("highlight_title ~p", [Reason])),
            <<>>;
        [Ret] ->
            Ret
    end,
    if byte_size(R) == 0 -> Name; true -> binary_to_list(R) end.

highlight_files(Key, Files) when is_list(Files) ->
    {Names, Lens} = lists:unzip(Files),
    BNames = [list_to_binary(Name) || Name <- Names],
    case catch build_file_excerpts(list_to_binary(?INDEX), list_to_binary(Key), BNames, 800) of
        {'EXIT', Reason} ->
            ?E(?FMT("highlight_files ~p", [Reason])),
            Files;
        Rets ->
            {L1, L2} = lists:foldl(fun({BName, Name, Len}, Acc) ->
                {HList, NHList} = Acc,
                if byte_size(BName) == 0 ->
                    {HList, [{Name, Len}|NHList]};
                    true ->
                    {[{binary_to_list(BName), Len}|HList], NHList}
                end
            end, {[], []}, lists:zip3(Rets, Names, Lens)),
            L1 ++ L2
    end.

build_file_excerpts(_BIndex, _BKey, [], _Batch)->
    [];
build_file_excerpts(BIndex, BKey, BNames, Batch) when length(BNames) < Batch ->
    sphinx_excerpt:build_excerpt(BKey, BNames, BIndex);
build_file_excerpts(BIndex, BKey, BNames, Batch)->
    {SubNames, Rest} = lists:split(Batch, BNames),
    Rets = sphinx_excerpt:build_excerpt(BKey, SubNames, BIndex),
    Rets ++ case got_excerpt(Rets) of
        true -> 
            [<<>> || _ <- Rest];
        false ->
            build_file_excerpts(BIndex, BKey, Rest, Batch)
    end.
    
got_excerpt([]) ->
    false;
got_excerpt([S|_]) when byte_size(S) > 0 ->
    true;
got_excerpt([_S|R]) ->
    got_excerpt(R).
