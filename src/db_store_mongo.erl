%%
%% db_store_mongo.erl
%% Kevin Lynx
%% 06.16.2013
%%
-module(db_store_mongo).
-include("vlog.hrl").
-export([init/2,
		 close/1,
		 insert/5,
		 unsafe_insert/2,
		 count/1,
		 inc_announce/2,
		 exist/2,
		 index/2,
		 search_newest_top_by_date/3,
		 search_announce_top/2,
		 search_recently/2,
		 search_newest_top/3,
		 search/2]).
-export([decode_torrent_item/1]).
-compile(export_all).
-define(DBNAME, torrents).
-define(COLLNAME, hashes).
-define(SEARCH_COL, name_array).

init(Host, Port) ->
	{ok, Conn} = mongo_connection:start_link({Host, Port}),
	?I(?FMT("connect mongodb ~p:~p success", [Host, Port])),
	init(Conn),
	Conn.

init(Conn) ->
	enable_text_search(Conn),
	ensure_search_index(Conn),
	ensure_numid_index(Conn),
	ok.

close(Conn) ->
	mongo_connection:stop(Conn).

count(Conn) ->
	mongo_do_slave(Conn, fun() ->
		mongo:count(?COLLNAME, {})
	end).

exist(Conn, Hash) when is_list(Hash) ->
	case find_exist(Conn, Hash) of
		{} -> false;
		_ -> true
	end.

% {Rets, {Found, CostTime}}
search(Conn, Key) when is_list(Key) ->
	BinColl = list_to_binary(atom_to_list(?COLLNAME)),
	BinKey = list_to_binary(Key),
	Ret = mongo_do_slave(Conn, fun() ->
		mongo:command({text, BinColl, search, BinKey})
	end),
	{decode_search(Ret), decode_search_stats(Ret)}.

% deprecated
search_announce_top(Conn, Count) ->
	Sel = {'$query', {}, '$orderby', {announce, -1}},
	List = mongo_do_slave(Conn, fun() ->
		% mongodb-erlang does not provide cursor.limit()/sort() functions, wired
		% but it work here
		Cursor = mongo:find(?COLLNAME, Sel, [], 0, Count), 
		mongo_cursor:rest(Cursor)
	end),
	[decode_torrent_item(Item) || Item <- List].
 
% deprecated
% db.hashes.find({$query:{},$orderby:{created_at: 1}}).limit(10);
search_recently(Conn, Count) ->
	Sel = {'$query', {}, '$orderby', {created_at, -1}},
	List = mongo_do_slave(Conn, fun() ->
		Cursor = mongo:find(?COLLNAME, Sel, [], 0, Count), 
		mongo_cursor:rest(Cursor)
	end),
	[decode_torrent_item(Item) || Item <- List].

% deprecated
search_newest_top(Conn, Count, DaySecs) ->	
	Sel = {'$query', {created_at, {'$gt', DaySecs}}, '$orderby', {announce, -1}},
	List = mongo_do_slave(Conn, fun() ->
		Cursor = mongo:find(?COLLNAME, Sel, [], 0, Count), 
		mongo_cursor:rest(Cursor)
	end),
	[decode_torrent_item(Item) || Item <- List].

% use hash_date to search, based on date index, so the search speed is very fast
search_newest_top_by_date(Conn, Count, DaySecs) ->
	Hashes = db_daterange:lookup(Conn, DaySecs, Count),
	Infos = [day_info(Conn, Hash, Req) || {Hash, Req} <- Hashes],
	lists:filter(fun(E) -> E /= {} end, Infos).

day_info(Conn, Hash, Req) ->
	case index(Conn, Hash) of
		{} -> {};
		Info ->
			setelement(4, Info, Req)
	end.

index(Conn, Hash) when is_list(Hash) ->
	Ret = mongo_do_slave(Conn, fun() ->
		mongo:find_one(?COLLNAME, {'_id', list_to_binary(Hash)})
	end),
	case Ret of 
		{} -> {};
		{Torrent} -> decode_torrent_item(Torrent)
	end.

insert(Conn, Hash, Name, Length, Files) when is_list(Hash) ->
	NewDoc = create_torrent_desc(Conn, Hash, Name, Length, 1, Files),
	db_daterange:insert(Conn, Hash),
	mongo_do(Conn, fun() ->
		% the doc may already exist because the other process has inserted before
		Sel = {'_id', list_to_binary(Hash)},
		mongo:update(?COLLNAME, Sel, NewDoc, true)
	end).

unsafe_insert(Conn, Tors) when is_list(Tors) ->
	Docs = [create_torrent_desc(Conn, Hash, Name, Length, 1, Files) || 
				{Hash, Name, Length, Files} <- Tors],
	mongo:do(unsafe, master, Conn, ?DBNAME, fun() ->
		mongo:insert(?COLLNAME, Docs)
	end).

inc_announce(Conn, Hash) when is_list(Hash) ->
	inc_announce(Conn, list_to_binary(Hash));
	
inc_announce(Conn, Hash) when is_binary(Hash) ->
	% damn, mongodb-erlang doesnot support update a field for an object,
	% `findAndModify` works but it will change `announce' datatype to double
	Cmd = {findAndModify, ?COLLNAME, query, {'_id', Hash}, 
		update, {'$inc', {announce, 1}}, fields, {'_id', 1}, % not specifed or {} will return whole object
		new, false},
	Ret = mongo_do(Conn, fun() ->
		mongo:command(Cmd)
	end),
	case Ret of
		{value, undefined, ok, 1.0} -> false;
		{value, _Obj, lastErrorObject, {updatedExisting, true, n, 1}, ok, 1.0} -> 
			db_daterange:insert(Conn, binary_to_list(Hash)),
			true;
		_ -> false
	end.

ensure_search_index(Conn) ->
	Spec = {key, {?SEARCH_COL, <<"text">>}},
	mongo_do(Conn, fun() ->
		mongo:ensure_index(?COLLNAME, Spec)
	end).

-ifdef(SPHINX).
ensure_numid_index(Conn) ->
	io:format("sphinx is enabled, ensure numid index for sphinx~n", []),
	Spec = {key, {numid, 1}},
	mongo_do(Conn, fun() ->
		mongo:ensure_index(?COLLNAME, Spec)
	end).
-else.
ensure_numid_index(_Conn) ->
	io:format("sphinx is disabled, use mongodb text search instead~n", []).
-endif.

% not work
enable_text_search(Conn) ->
	Cmd = {setParameter, 1, textSearchEnabled, true},
	mongo:do(safe, master, Conn, admin, fun() ->
		mongo:command(Cmd)
	end).

-ifdef(SPHINX).
create_torrent_desc(Conn, Hash, Name, Length, Announce, Files) ->
	{'_id', list_to_binary(Hash),
      % steven told me it's necessary for sphinx, what if the doc already exists ?
	  numid, db_system:get_torrent_id(Conn), 
	  name, list_to_binary(Name),
	  length, Length,
	  created_at, time_util:now_seconds(),
	  announce, Announce,
	  files, encode_file_list(Files)}.
-else.
create_torrent_desc(_Conn, Hash, Name, Length, Announce, Files) ->
	NameArray = seg_text(Name, Files),
	{'_id', list_to_binary(Hash),
	  name, list_to_binary(Name),
	  name_array, NameArray,
	  length, Length,
	  created_at, time_util:now_seconds(),
	  announce, Announce,
	  files, encode_file_list(Files)}.

seg_text(Name, Files) ->
	FullName = lists:foldl(fun({S, _}, Acc) ->
		Acc ++ " " ++ S
	end, Name, Files),
	seg_text(FullName).

seg_text(FullName) ->
	case config:get(use_rmmseg, false) of
		false -> list_to_binary(FullName);
		true ->
			rmmseg:seg_space(list_to_binary(FullName))
	end.
-endif.

% {file1, {name, xx, length, xx}, file2, {name, xx, length, xx}}
encode_file_list(Files) ->
	Keys = ["file"++integer_to_list(Index) || Index <- lists:seq(1, length(Files))],
	Generator = lists:zip(Keys, Files),
	list_to_tuple(lists:flatten([[list_to_atom(Key), {name, list_to_binary(Name), length, Length}]
		|| {Key, {Name, Length}} <- Generator])).

find_exist(Conn, Hash) ->
	mongo_do(Conn, fun() ->
		mongo:find_one(?COLLNAME, hash_selector(Hash))
	end).

mongo_do(Conn, Fun) ->
	mongo:do(safe, master, Conn, ?DBNAME, Fun).

mongo_do_slave(Conn, Fun) ->
	mongo:do(safe, slave_ok, Conn, ?DBNAME, Fun).

% TODO: replace this with {'_id', ID}
hash_selector(Hash) ->
	Expr = lists:flatten(io_lib:format("this._id == '~s'", [Hash])),
	{'$where', list_to_binary(Expr)}.

decode_search_stats(Rets) ->
	{Stats} = bson:lookup(stats, Rets),
	{Found} = bson:lookup(nfound, Stats),
	{Cost} = bson:lookup(timeMicros, Stats),
	{Scanned} = bson:lookup(nscanned, Stats),
	{Found, Cost, Scanned}.

decode_search(Rets) ->
	case bson:lookup(results, Rets) of
		{} ->
			[];
		{List} ->
			[decode_ret_item(Item) || Item <- List]
	end.

decode_ret_item(Item) ->
	{Torrent} = bson:lookup(obj, Item),
	decode_torrent_item(Torrent).

% {single, Hash, {Name, Length}, RequestCount, CreatedAt}
% {multi, Hash, {Name, Files}, RequestCount, CreatedAt}
decode_torrent_item(Torrent) ->	
	{BinHash} = bson:lookup('_id', Torrent),
	Hash = binary_to_list(BinHash),
	{BinName} = bson:lookup(name, Torrent),
	Name = binary_to_list(BinName),
	{Length} = bson:lookup(length, Torrent),
	{CreatedT} = bson:lookup(created_at, Torrent),
	ICreatedAt = round(CreatedT),
	{Announce} = bson:lookup(announce, Torrent),
	IA = round(Announce), % since announce maybe double in mongodb
	case bson:lookup(files, Torrent) of
		{{}} ->
			{single, Hash, {Name, Length}, IA, ICreatedAt};
		{Files} ->
			{multi, Hash, {Name, decode_files(tuple_to_list(Files))}, IA, ICreatedAt}
	end.
 
decode_files(Files) ->
	decode_file(Files).

decode_file([_|[File|Rest]]) ->
	{BinName} = bson:lookup(name, File),
	Name = binary_to_list(BinName),
	{Length} = bson:lookup(length, File),
	[{Name, Length}] ++ decode_file(Rest);

decode_file([]) ->
	[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_insert() ->
	Conn = init(localhost, 27017),
	insert(Conn, "7C6932E7EC1CF5B00AE991871E57B2375DADA5A0", "movie 1", 128, []),
	insert(Conn, "AE94E340B5234C8410F37CFA7170F8C5657ECE5D", "another movie name", 0, 
		[{"subfile-a", 100}, {"subfile-b", 80}]),
	insert(Conn, "0F1B5BE407E130AEEA8AB2964F5100190086ED93", "oh it work", 2456, []),
	close(Conn).

test_content(Fun) ->
	Conn = init(localhost, 27017),
	Ret = Fun(Conn),
	close(Conn),
	Ret.

test_ensureidx() ->
	test_content(fun(Conn) ->
		enable_text_search(Conn),
		ensure_search_index(Conn)
	end).

test_search(Key) ->
	test_content(fun(Conn) ->
		search(Conn, Key)
	end).

test_tmpsearch(Key) ->
	test_content(fun(Conn) ->
		BinColl = list_to_binary(atom_to_list(?COLLNAME)),
		BinKey = list_to_binary(Key),
		Ret = mongo_do(Conn, fun() ->
			mongo:command({text, BinColl, search, BinKey})
		end),
		Ret
	end).

test_count() ->
	test_content(fun(Conn) ->
		count(Conn)
	end).

test_find_top() ->
	test_content(fun(Conn) ->
		search_announce_top(Conn, 2)
	end).

test_announce() ->
	Hash = "6605993570A4A74C89FD02355333F3A86BEE6C1F",
	test_content(fun(Conn) ->
		inc_announce(Conn, Hash)
	end).

test_index(Hash) ->
	test_content(fun(Conn) ->
		index(Conn, Hash)
	end).

test_insertdate(Hash) ->
	test_content(fun(Conn) ->
		db_daterange:insert(Conn, Hash)
	end).

-ifdef(SPHINX).
test_compile() ->
    io:format("sphinx enabled~n", []).
-else.
test_compile() ->
    io:format("sphins disabled~n", []).
-endif.

