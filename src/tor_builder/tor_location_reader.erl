%%
%% tor_location_reader.erl
%% Kevin Lynx
%% 07.08.2013
%%
-module(tor_location_reader).
-behaviour(gen_server).
-include("vlog.hrl").
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1, 
		 get/1,
		 ensure_status_files/1,
		 stop/0]).
-record(state, {dir, fp, sum, position, maxpos, lastPrintPos = 0}).

start_link(RootDir) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [RootDir], []).

stop() ->
	gen_server:cast(srv_name(), stop).

get(Count) ->
	gen_server:call(srv_name(), {read, Count}).

ensure_status_files(RootDir) ->
	Index = RootDir ++ "index.txt",
	Status = RootDir ++ "index_status.txt",
	(filelib:is_regular(Index)) and (filelib:is_regular(Status)).

srv_name() ->
	?MODULE.

init([RootDir]) ->
	{ok, FP} = file:open(RootDir ++ "index.txt", [read]),
	{ok, [Status]} = file:consult(RootDir ++ "index_status.txt"),
	Pos = proplists:get_value(position, Status),
	Sum = proplists:get_value(sum, Status),
	{ok, MaxPos} = file:position(FP, eof),
	file:position(FP, Pos),
	?I(?FMT("read status file ok ~p, ~p", [Pos, MaxPos])),
	{ok, #state{fp = FP, position = Pos, maxpos = MaxPos, sum = Sum, dir = RootDir}, 0}.

terminate(_, #state{fp = FP} = State) ->
	file:close(FP),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(timeout, State) ->
	{noreply, State};

handle_info(M, State) ->
	?W(?FMT("unhandled message ~p", [M])),
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call({read, Count}, _From, State) ->
	#state{dir = RootDir, sum = Sum, fp = FP, 
		lastPrintPos = OldPrPos, maxpos = MaxPos, position = OldPos} = State,
	List = do_read(FP, Count),
	{NewPos, Names, PrintPos} = case length(List) == 0 of
		true when OldPos == MaxPos ->
			PrPos = display_progress(MaxPos, MaxPos, OldPrPos),
			update_status(RootDir, FP, Sum, OldPos),
			% to avoid multiple display 100%
			{MaxPos + 1, [], PrPos}; 
		true ->
			{MaxPos + 1, [], OldPrPos};
		false ->
			PrPos = display_progress(MaxPos, OldPos, OldPrPos),
			Pos = update_status(RootDir, FP, Sum, OldPos),
			{Pos, List, PrPos}
	end,
	{reply, Names, State#state{position = NewPos, lastPrintPos = PrintPos}};

handle_call(_, _From, State) ->
	{noreply, State}.

display_progress(MaxPos, CurPos, OldPrPos) 
when 100 * (CurPos - OldPrPos) / MaxPos >= 10; MaxPos == CurPos ->
	io:format("--> ~.2f%~n", [CurPos * 100 / MaxPos]),
	CurPos;

display_progress(_MaxPos, _CurPos, OldPrPos) ->
	OldPrPos.

do_read(_, 0) ->
	[];
do_read(FP, Count) ->
	case io:get_line(FP, "") of
		eof -> [];
		Line -> [strip_lf(Line) | do_read(FP, Count - 1)]
	end.

strip_lf(S) ->
	lists:sublist(S, length(S) - 1).

% when some error occurs, the processor can continue to work from old pos
update_status(RootDir, FP, Sum, OldPos) ->
	{ok, Pos} = file:position(FP, cur),
	Status = [{sum, Sum}, {position, OldPos}],	
	?I(?FMT("update status ~p", [Status])),
	file:write_file(RootDir ++ "index_status.txt", io_lib:fwrite("~p.\n",[Status])),
	Pos.



