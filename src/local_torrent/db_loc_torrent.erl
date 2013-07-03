%%
%% db_loc_torrent.erl
%% Kevin Lynx
%% 07.03.2013
%%
-module(db_loc_torrent).
-include("db_common.hrl").
-define(DBNAME, torfiles).
-define(COLLNAME, torrents).
-export([load_hash/1,
		 save/3,
		 load/2]).
-export([test_load/1]).

% return MagHash as a list
load_hash(Conn) ->
	% db.runCommand({findAndModify:"wait_download",query:{cached:{$ne:1}},update:{$inc:{cached:1}}})	
	Cmd = {findAndModify, ?HASH_DOWNLOAD_COLL, query, {cached, {'$ne', 1}}, 
		update, {'$set', {cached, 1}}, fields, {hash, 1}},
	Ret = mongo:do(safe, master, Conn, ?HASH_DBNAME, fun() ->
		mongo:command(Cmd)
	end),
	case bson:lookup(value, Ret) of
		{undefined} ->
			[];
		{} ->
			[];
		{Obj} ->
			{BinHash} = bson:lookup(hash, Obj),
			binary_to_list(BinHash)
	end.

save(Conn, MagHash, Content) ->
	Doc = torrent_doc(MagHash, Content),
	mongo:do(unsafe, master, Conn, ?DBNAME, fun() ->
		Sel = {'_id', list_to_binary(MagHash)},
		mongo:update(?COLLNAME, Sel, Doc, true)
	end).

% load a torrent file
load(Conn, MagHash) when is_list(MagHash) ->
	Ret = mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		Sel = {'_id', list_to_binary(MagHash)},
		mongo:find_one(?COLLNAME, Sel)
	end),
	case Ret of
		{} -> 
			not_found;
		{Doc} ->
			{{bin, bin, Content}} = bson:lookup(content, Doc),
			Content
	end.

torrent_doc(MagHash, Content) when is_binary(Content), is_list(MagHash) ->
	{'_id', list_to_binary(MagHash),
	content, {bin, bin, Content}}.

%%
test_load(MagHash) ->
	{ok, Conn} = mongo_connection:start_link({localhost, 27017}),
	Content = load(Conn, MagHash),
	Name = MagHash ++ ".torrent",
	file:write_file(Name, Content),
	Name.


