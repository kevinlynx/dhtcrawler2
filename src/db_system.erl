%%
%% db_system.erl
%% Kevin Lynx
%% 06.28.2013
%%
-module(db_system).
-export([load_batch_index/1,
		 inc_batch_rindex/1,
		 inc_batch_windex/1]).
-export([stats_new_saved/1,
		 stats_updated/1,
		 stats_query_inserted/2,
		 stats_day_at_slave/2,
		 stats_filtered/1,
		 stats_get_peers/1]).
-export([get_torrent_id/1]).
-compile(export_all).
-define(DBNAME, dht_system).
-define(COLLNAME, system).
-define(HASH_BATCH_KEY, <<"hashbatch">>).
-define(STATS_COLLNAME, stats).
-define(TORRENT_ID_KEY, <<"torrentid">>).

% increase the seed and return the new id
get_torrent_id(Conn) ->
	Cmd = {findAndModify, ?COLLNAME, query, {'_id', ?TORRENT_ID_KEY},
		update, {'$inc', {seed, 1}}, new, true, upsert, true},
	Ret = mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:command(Cmd)
	end),
	case bson:lookup(value, Ret) of
		{undefined} ->
			1;
		{} ->
			1;
		{Obj} ->
			{Seed} = bson:lookup(seed, Obj),
			Seed
	end.

%% batch index
inc_batch_rindex(Conn) ->
	inc_batch_index(Conn, read_index).

inc_batch_windex(Conn) ->
	inc_batch_index(Conn, write_index).

inc_batch_index(Conn, Col) ->
	Cmd = {findAndModify, ?COLLNAME, query, {'_id', ?HASH_BATCH_KEY}, 
		update, {'$inc', {Col, 1}}, new, true},
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:command(Cmd)
	end).

load_batch_index(Conn) ->
	Doc = case find_exist_batch_index(Conn) of
		{} ->
			NewDoc = create_batch_index(0, 0),
			mongo:do(safe, master, Conn, ?DBNAME, fun() ->
				mongo:insert(?COLLNAME, NewDoc)
			end),
			NewDoc;
		{Exist} ->
			Exist
	end,
	{RIndex} = bson:lookup(read_index, Doc),
	{WIndex} = bson:lookup(write_index, Doc),
	{RIndex, WIndex}.

find_exist_batch_index(Conn) ->
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:find_one(?COLLNAME, {'_id', ?HASH_BATCH_KEY})
	end).

create_batch_index(WIndex, RIndex) ->
	{'_id', ?HASH_BATCH_KEY, read_index, WIndex, write_index, RIndex}.

%% stats collection
stats_new_saved(Conn) ->
	stats_inc_field(Conn, new_saved).

stats_updated(Conn) ->
	stats_inc_field(Conn, updated).

% already processes query
stats_get_peers(Conn) ->
	stats_inc_field(Conn, get_peers).

% all queries, not processed
stats_query_inserted(Conn, Count) ->
	stats_inc_field(Conn, get_peers_query, Count).

stats_cache_query_inserted(Conn, Count) ->
	stats_inc_field(Conn, inserted_query, Count).

stats_filtered(Conn) ->
	stats_inc_field(Conn, filter_hash).

stats_inc_field(Conn, Filed) ->
	stats_inc_field(Conn, Filed, 1).

stats_inc_field(Conn, Field, Inc) ->
	TodaySecs = time_util:now_day_seconds(),
	Cmd = {findAndModify, ?STATS_COLLNAME, query, {'_id', TodaySecs}, 
		upsert, true, update, {'$inc', {Field, Inc}}, field, {'_id', 1}},
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:command(Cmd)
	end).

stats_day_at_slave(Conn, DaySec) ->
	Ret = mongo:do(safe, slave_ok, Conn, ?DBNAME, fun() ->
		mongo:find_one(?STATS_COLLNAME, {'_id', DaySec})
	end),
	case Ret of
		{} -> {};
		{Doc} -> Doc
	end.

%%
test_torrent_id() ->
	{ok, Conn} = mongo_connection:start_link({localhost, 27017}),
	ID = get_torrent_id(Conn),
	ID.
