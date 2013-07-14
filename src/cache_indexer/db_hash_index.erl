%%
%% db_hash_index.erl
%% Kevin Lynx
%% 07.14.2013
%%
-module(db_hash_index).
-export([insert/2, exist/2]).
-define(DBNAME, hash_cache).
-define(COLLNAME, hashes).

insert(Conn, Hash) when is_list(Hash) ->
	case catch do_insert(Conn, Hash) of
		{'EXIT', _} -> failed;
		_ -> ok
	end.

do_insert(Conn, Hash) ->
	Doc = {'_id', list_to_binary(Hash)},
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:insert(?COLLNAME, Doc)
	end).

exist(Conn, Hash) when is_list(Hash) ->
	Sel = {'_id', list_to_binary(Hash)},
	{Doc} = mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:find_one(?COLLNAME, Sel)
	end),
	Doc == {}.

