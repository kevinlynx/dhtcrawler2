%%
%% string_util.erl
%% Kevin Lynx
%% 
-module(string_util).
-export([format/2]).

format(Fmt, Arg) when is_list(Fmt), is_list(Arg) ->
	lists:flatten(io_lib:format(Fmt, Arg)).
	
