%%
%% xmerl_xml_cdata.erl
%% Kevin Lynx
%% To fix erlang xmerl_xml not support `cdata' bug, damn it!
%%
-module(xmerl_xml_cdata).
-export(['#xml-inheritance#'/0]).
-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).
-import(xmerl_lib, [markup/3, empty_tag/2]).
-include_lib("xmerl/include/xmerl.hrl").


'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
%io:format("Text=~p~n",[Text]),
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, [#xmlAttribute{name=prolog,value=V}], [], _E) ->
    [V,Data];
'#root#'(Data, _Attrs, [], _E) ->
    ["<?xml version=\"1.0\"?>", Data].


%% The '#element#' function is the default handler for XML elements.

'#element#'(Tag, [], Attrs, _Parents, _E) ->
%io:format("Empty Tag=~p~n",[Tag]),
    empty_tag(Tag, Attrs);
'#element#'(Tag, Data, Attrs, _Parents, _E) ->
%io:format("Tag=~p~n",[Tag]),
    markup(Tag, Attrs, Data).

export_text(T) ->
    export_text(T, []).

export_text([$' | T], Cont) ->
	"&apos;" ++ export_text(T, Cont);
export_text([$" | T], Cont) ->
	"&quot;" ++ export_text(T, Cont);
export_text([$< | T] = Text, Cont) ->
	case (lists:prefix("<![CDATA[", Text)) and (lists:suffix("]]>", Text)) of
		true -> Text;
		false ->
    		"&lt;" ++ export_text(T, Cont)
    end;
export_text([$> | T], Cont) ->
    "&gt;" ++ export_text(T, Cont);
export_text([$& | T], Cont) ->
    "&amp;" ++ export_text(T, Cont);
export_text([C | T], Cont) when is_integer(C) ->
    [C | export_text(T, Cont)];
export_text([T | T1], Cont) ->
    export_text(T, [T1 | Cont]);
export_text([], [T | Cont]) ->
    export_text(T, Cont);
export_text([], []) ->
    [];
export_text(Bin, Cont) ->
    export_text(binary_to_list(Bin), Cont).
