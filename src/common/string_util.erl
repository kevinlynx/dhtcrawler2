%%
%% string_util.erl
%% Kevin Lynx
%% 
-module(string_util).
-compile(export_all).
-export([format/2, strip_invalid_unicode/1]).

format(Fmt, Arg) when is_list(Fmt), is_list(Arg) ->
	lists:flatten(io_lib:format(Fmt, Arg)).
	
% strip these unicode control characters
strip_invalid_unicode(L) when is_list(L) ->
	binary_to_list(strip_invalid_unicode(list_to_binary(L)));
strip_invalid_unicode(<<>>) ->
	<<>>;
strip_invalid_unicode(<<C/utf8, R/binary>>) ->
	case is_valid_unicode(C) of
		true ->
			RR = strip_invalid_unicode(R),
			<<C/utf8, RR/binary>>;
		false ->
			strip_invalid_unicode(R)
	end;
strip_invalid_unicode(<<_, R/binary>>) ->
	strip_invalid_unicode(R).
	
is_valid_unicode(C) when C < 16#20 ->
	false;
is_valid_unicode(C) when C >= 16#7f, C =< 16#ff ->
	false;
is_valid_unicode(_) ->
	true.


