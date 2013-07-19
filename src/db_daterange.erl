%%
%% db_daterange.erl
%% Kevin Lynx
%% 07.10.2013
%% To track the most recently hashes
%%
-module(db_daterange).
-export([insert/3,
		 lookup/3]).
-export([start_link/1,
		 stop/0]).
-define(DBNAME, hash_date).
-define(COLLNAME, hashes).
-define(DATE_COL, date).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-record(state, {startday, dbpool, counter}).
-define(CHECK_BOUNDARY, 1000).
-define(DEL_DAY_BEFORE_SECS, 5*24*60*60).

% query by date (in seconds), build the index
ensure_date_index(Conn) ->
	Spec = {key, {?DATE_COL, 1}},
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:ensure_index(?COLLNAME, Spec)
	end).

% '_id': Hash, date: DaySecs, reqs: RequestCount
insert(Conn, Hash, ReqCnt) when is_list(Hash) ->
	DaySecs = time_util:now_day_seconds(),
	BHash = list_to_binary(Hash),
	% only record today new inserted torrent
	Cmd = {findAndModify, ?COLLNAME, query, {'_id', BHash}, % upsert, true,
		update, {'$inc', {reqs, ReqCnt}, '$set', {?DATE_COL, DaySecs}}, fields, {'_id', 1}},
	IRet = mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:command(Cmd)
	end),
	gen_server:cast(srv_name(), insert),
	IRet.

% [{ListHash, Req}, {ListHash, Req}]
lookup(Conn, DaySecs, Count) ->
	Sel = {'$query', {date, DaySecs}, '$orderby', {reqs, -1}},
	List = mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		Cursor = mongo:find(?COLLNAME, Sel, {'_id', 1, reqs, 1}, 0, Count), 
		mongo_cursor:rest(Cursor)
	end),
	[decode_hash(Doc) || Doc <- List].

decode_hash(Doc) ->
	{ID} = bson:lookup('_id', Doc),
	{Req} = bson:lookup(reqs, Doc),
	{binary_to_list(ID), Req}.
		
% delete all oldest hashes 
try_delete_oldest(Conn) ->
	TodaySecs = time_util:now_day_seconds(),
	DelDay = TodaySecs - ?DEL_DAY_BEFORE_SECS,
	Sel = {date, {'$lte', DelDay}},
	mongo:do(safe, master, Conn, ?DBNAME, fun() ->
		mongo:delete(?COLLNAME, Sel)
	end).

%%%%
start_link(DBPool) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [DBPool], []).

stop() ->
	gen_server:cast(srv_name(), stop).

srv_name() ->
	?MODULE.

init([Pool]) ->
	Conn = mongo_pool:get(Pool),
	ensure_date_index(Conn),
	Today = time_util:now_day_seconds(),
	{ok, #state{startday = Today, dbpool = Pool, counter = 0}}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

% not care duplicated, check when there is more than 1 day
handle_cast(insert, State) ->
	#state{startday = StartDay, dbpool = Pool, counter = Count} = State,
	NowDay = time_util:now_day_seconds(),
	{NewCount, NewStart} = case Count > ?CHECK_BOUNDARY of
		true when NowDay > StartDay ->
			Conn = mongo_pool:get(Pool),
			try_delete_oldest(Conn),
			{0, NowDay};
		true ->
			{0, StartDay};
		false ->
			{Count + 1, StartDay}
	end,
	{noreply, State#state{startday = NewStart, counter = NewCount}};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

