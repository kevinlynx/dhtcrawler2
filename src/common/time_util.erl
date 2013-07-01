%%
%% time_util.erl
%% Kevin Lynx
%% 06.18.2013
%%
-module(time_util).
-export([now_seconds/0, 
		 diff_milsecs/2,
		 seconds_to_local_time/1,
		 now_day_seconds/0]).
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

