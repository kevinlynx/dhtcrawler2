%%
%% index_file.erl
%% Kevin Lynx
%% 07.14.2013
%%
-module(index_file).
-export([start/2]).
-export([worker_run/3]).
-define(PRINT_STEP, 5000*41).

start(Conn, FileName) ->
	spawn_link(?MODULE, worker_run, [self(), Conn, FileName]).

load_position(Name) ->
	StatusFile = Name ++ ".sta",
	Pos = case file:consult(StatusFile) of
		{ok, []} -> 0;
		{ok, [Status]} ->
			proplists:get_value(position, Status);
		{error, _} ->
			0
	end,
	Pos.

save_position(Name, Pos) ->
	StatusFile = Name ++ ".sta",
	Status = [{name, Name}, {position, Pos}],
	file:write_file(StatusFile, io_lib:fwrite("~p.\n",[Status])).

worker_run(Parent, Conn, FileName) ->
	Pos = load_position(FileName),
	io:format("start to process ~s from ~p~n", [FileName, Pos]),
	{ok, FP} = file:open(FileName, [read]),
	{ok, MaxPos} = file:position(FP, eof),
	file:position(FP, Pos),
	Step = MaxPos div 10, % every 10% to display progress
	CheckStep = if Step < ?PRINT_STEP -> ?PRINT_STEP; true -> Step end,
	Sum = process_hash(Conn, FileName, FP, Pos, MaxPos, CheckStep),
	Parent ! {worker_done, self(), FileName},
	file:close(FP),
	io:format("Index file ~s done, ~p hashes~n", [FileName, Sum]).

process_hash(Conn, FileName, FP, PrintPos, MaxPos, CheckStep) ->
	case io:get_line(FP, "") of
		eof -> 
			check_progress(FileName, MaxPos, 0, MaxPos, CheckStep),
			0;
		Line ->
			save_hash(Conn, strip_lf(Line)),
			{ok, Pos} = file:position(FP, cur),
			NewPrintPos = check_progress(FileName, Pos, PrintPos, MaxPos, CheckStep),
			1 + process_hash(Conn, FileName, FP, NewPrintPos, MaxPos, CheckStep)
	end.

check_progress(FileName, Pos, PrintPos, MaxPos, CheckStep) when Pos >= PrintPos ->
	save_position(FileName, Pos),
	io:format("~s -> ~b%~n", [FileName, 100 * Pos div MaxPos]),
	PrintPos + CheckStep;
check_progress(_, _, PrintPos, _, _) ->
	PrintPos.

strip_lf(S) ->
	lists:sublist(S, length(S) - 1).

save_hash(Conn, Hash) when length(Hash) == 40 ->
	db_hash_index:insert(Conn, Hash);

save_hash(_, _) ->
	invalid.

