%%
%% sphinx_id.erl
%% Kevin Lynx
%%
-module(sphinx_id).
-export([fromhash/1, tohash/1]).
-compile(export_all).

fromhash(Hash) when is_list(Hash), length(Hash) == 40 ->
	B = hex:hexstr_to_bin(Hash),
	<<H1:32/integer, H2:32/integer, H3:32/integer, H4:32/integer, H5:32/integer>> = B,
	{H1, H2, H3, H4, H5}.


tohash({H1, H2, H3, H4, H5}) ->
	B = <<H1:32/integer, H2:32/integer, H3:32/integer, H4:32/integer, H5:32/integer>>,
	string:to_upper(hex:bin_to_hexstr(B)).

