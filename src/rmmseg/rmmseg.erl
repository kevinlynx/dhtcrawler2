%%
%% rmmseg.erl
%% Kevin Lynx
%% 
-module(rmmseg).
-export([init/0, 
         load_dicts/0,
         seg_space/1,
         load_dicts/2, 
         seg/1]).
-onload(init/0).
-compile(export_all).

init() ->
    File = in_priv_path("rmmseg"),
	ok = erlang:load_nif(File, 0).

load_dicts(_CharFile, _WordFile) ->
	not_loaded.

seg(_BStr) ->
	not_loaded.

load_dicts() ->
    Chars = in_priv_path("chars.dic"),
    Words = in_priv_path("words.dic"),
    load_dicts(Chars, Words).

seg_space(BStr) when is_binary(BStr) ->
	List = rmmseg:seg(BStr),
	Ret = lists:foldl(fun(E, Acc) ->
            case Acc == <<>> of 
                true -> E;
                false ->
		            <<Acc/binary, " ", E/binary>>
            end
    end, <<>>, List),
    Ret.

in_priv_path(Name) ->
	filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", Name]).

%% 
sample() ->
    not_loaded.

