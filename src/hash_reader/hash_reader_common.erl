%%
%% hash_reader_common.erl
%% Kevin Lynx
%% 07.21.2013
%%
-module(hash_reader_common).
-include("db_common.hrl").
-export([get_req_cnt/1,
		 on_updated/1,
		 load_delete_doc/2]).

get_req_cnt(Doc) ->
	case bson:lookup(req_cnt, Doc) of
		{} -> 0;
		{R} -> R
	end.

on_updated(Conn) ->
	% `get_peers' here means we have processed a request
	db_system:stats_get_peers(Conn),
	% also increase the updated counter
	db_system:stats_updated(Conn),
	hash_reader_stats:handle_update().

load_delete_doc(Conn, Col) ->
	Cmd = {findAndModify, Col, fields, {}, remove, true},
	Ret = mongo:do(safe, master, Conn, ?HASH_DBNAME, fun() ->
		mongo:command(Cmd)
	end),
	case Ret of
		{value, undefined, ok, 1.0} -> {};
		{value, Obj, lastErrorObject, _, ok, 1.0} -> {Obj}
	end.

