%%
%% index_file.erl
%% Kevin Lynx
%% 07.14.2013
%%
-module(index_file).
-export([start/2]).
-export([worker_run/3]).

start(Conn, FileName) ->
	spawn_link(?MODULE, worker_run, [self(), Conn, FileName]).

load_position(Name) ->
	StatusFile = Name ++ ".sta",
	Pos = case file:consult(StatusFile) of
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
	file:position(FP, Pos),
	Sum = process_hash(Conn, FileName, FP),
	Parent ! {worker_done, self(), FileName},
	file:close(FP),
	io:format("Index file ~s done, ~p hashes~n", [FileName, Sum]).

process_hash(Conn, FileName, FP) ->
	case io:get_line(FP, "") of
		eof -> 0;
		Line ->
			save_hash(Conn, strip_lf(Line)),
			{ok, Pos} = file:position(FP, cur),
			save_position(FileName, Pos),
			1 + process_hash(Conn, FileName, FP)
	end.

strip_lf(S) ->
	lists:sublist(S, length(S) - 1).

save_hash(Conn, Hash) when length(Hash) == 40 ->
	db_hash_index:insert(Conn, Hash);

save_hash(_, _) ->
	invalid.

