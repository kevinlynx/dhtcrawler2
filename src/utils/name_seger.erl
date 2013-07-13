%%
%% name_seger.erl
%% Kevin Lynx
%% segment name by rmmseg 
%%
-module(name_seger).
-export([start/0]).
-define(DBNAME, torrents).
-define(COLLNAME, hashes).
-define(POOLNAME, db_pool).
-define(BATCHSIZE, 1000).

start_dep_apps() ->
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/mongodb/ebin"),
	Apps = [asn1, crypto, public_key, ssl, inets, bson, mongodb],	
	[application:start(App) || App <- Apps].

start() ->
	start_dep_apps(),
	rmmseg:init(),
	rmmseg:load_dicts(),
	IP = localhost,
	Port = 27017,
	mongo_sup:start_pool(?POOLNAME, 2, {IP, Port}),
	process().

process() ->
	Conn = mongo_pool:get(?POOLNAME),
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		Cursor = mongo:find(?COLLNAME, {}),
		process(Cursor, ok, 0)
	end).

process(Cursor, ok, Sum) ->
	print_stats(Sum),
	Ret = process_one(mongo_cursor:next(Cursor)),
	process(Cursor, Ret, Sum + 1);

process(_Cursor, stop, Sum) ->
	io:format("process done, total ~p~n", [Sum]),
	stop.

print_stats(Sum) ->
	case Sum rem 500 == 0 of
		true ->
			io:format(" -> ~p~n", [Sum]);
		false ->
			ok
	end.

commit(Hash, NameArray) when is_binary(Hash), is_binary(NameArray) ->
	Cmd = {findAndModify, ?COLLNAME, query, {'_id', Hash}, 
		update, {'$set', {name_array, NameArray}}, fields, {'_id', 1},
		new, false},
	mongo:command(Cmd).

process_one({}) ->
	stop;
process_one({Doc}) ->
	Torrent = db_store_mongo:decode_torrent_item(Doc),
	{Hash, NameArray} = seg_torrent(Torrent),
	commit(list_to_binary(Hash), NameArray),
	ok.

seg_torrent({single, Hash, {Name, _}, _, _}) ->
	{Hash, rmmseg:seg_space(list_to_binary(Name))};

seg_torrent({multi, Hash, {Name, Files}, _, _}) ->
	FullName = lists:foldl(fun({S, _}, Acc) ->
		Acc ++ " " ++ S
	end, Name, Files),
	{Hash, rmmseg:seg_space(list_to_binary(FullName))}.

