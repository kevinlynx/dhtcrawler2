%%
%% tor_name_seg.erl
%% Kevin Lynx
%% segment torrent name into words
%%
-module(tor_name_seg).
-include("vlog.hrl").
-export([init/0, seg_text/1]).

%% text_seg: [rmmseg, none, simple]
init() ->
	Method = config:get(text_seg, simple),
	io:format("text segment use `~p`~n", [Method]),
	do_init(Method).

seg_text(Text) when is_list(Text) ->
	Method = config:get(text_seg, simple),
	do_seg_text(Method, Text).

do_init(rmmseg) ->
	rmmseg:init(), 
	rmmseg:load_dicts();

do_init(none) ->
	io:format("warning: text segment `none` can NOT search by non-english text~n", []),
	ok;

do_init(simple) ->
	ok;

do_init(M) ->
	io:format("unknown text segment method ~p, only support [none, simple, rmmseg]~n", [M]),
	ok.

do_seg_text(none, Text) ->
	list_to_binary(Text);

do_seg_text(rmmseg, Text) ->
	rmmseg:seg_space(list_to_binary(Text));

do_seg_text(simple, Text) ->
	case string_split:split(Text) of
		{error, L, D} ->
			?E(?FMT("string split failed(error): ~p ~p", [L, D])),
			list_to_binary(Text);
		{incomplete, L, D} ->
			?E(?FMT("string split failed(incomplte): ~p ~p", [L, D])),
			list_to_binary(Text);
		{ok, R} -> 
			R 
	end.


