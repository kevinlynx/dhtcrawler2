%%
%% time_util.erl
%% Kevin Lynx
%% 06.18.2013
%%
-module(time_util).
-export([now_seconds/0, 
		 diff_milsecs/2,
		 seconds_to_local_time/1,
		 local_time_to_universal_time/1,
		 now_utc_time/0,
		 now_day_seconds/0,
		 date_time_string/1,
		 date_time_string/0,
		 date_time_stamp/0]).
-compile(export_all).

diff_milsecs(T1, T2) ->
	timer:now_diff(T1, T2) div 1000.

now_seconds() ->
	{Megasecs, Secs, Microsecs} = now(),
	(Megasecs * 1000000) + Secs + (Microsecs div 1000000).

seconds_to_local_time(Secs) ->
	{{Y, M, D}, Time} = calendar:gregorian_seconds_to_datetime(Secs),
 	calendar:universal_time_to_local_time({{Y + 1970, M, D}, Time}).

now_day_seconds() ->
 	{{Year, Month, Day}, _Time} = calendar:local_time(),
 	{{NY, NM, ND}, Time} = local_time_to_universal_time({{Year, Month, Day}, {0, 0, 0}}),
	calendar:datetime_to_gregorian_seconds({{NY - 1970, NM, ND}, Time}).

local_time_to_universal_time(Datetime) ->
	case calendar:local_time_to_universal_time_dst(Datetime) of
		[_, DateTimeUTC] ->
			DateTimeUTC;
		[DateTimeUTC] ->
			DateTimeUTC
	end.

now_utc_time() ->
	local_time_to_universal_time(calendar:local_time()).

date_time_string() ->
 	date_time_string(calendar:local_time()).

date_time_string({{Y, M, D}, {H, Min, Sec}}) ->
 	L = io_lib:format("~b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",
		[Y, M, D, H, Min, Sec]),
 	lists:flatten(L);
date_time_string(DaySecs) ->
	DateTime = seconds_to_local_time(DaySecs),
	date_time_string(DateTime).

date_time_stamp() ->
 	{{Y, M, D}, {H, Min, Sec}} = calendar:local_time(),
 	L = io_lib:format("~b~2..0b~2..0b~2..0b~2..0b~2..0b",
		[Y, M, D, H, Min, Sec]),
 	lists:flatten(L).
