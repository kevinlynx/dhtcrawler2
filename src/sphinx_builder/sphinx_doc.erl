%%
%% sphinx_doc.erl
%% 07.27.2013
%%
-module(sphinx_doc).
-include_lib("xmerl/include/xmerl.hrl").
-export([write_xml/2, element/6]).
-compile(export_all).
-define(PROLOG, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").  
-define(CR, #xmlText{value="\
	"}).

write_xml(File, Elems) ->
	Doc = {'sphinx:docset', [], [schema(), ?CR] ++ Elems},
	Content = xmerl:export_simple([Doc], xmerl_xml_cdata, [{prolog, ?PROLOG}]),	
	file:write_file(File, Content).

element(Hash, Name, Files, ID, Query, CreatedAt) when is_list(Hash), length(Hash) == 40 ->
	IDSet = idset(Hash),
	OtherAttrs = [{query, [], [integer_to_list(Query)]}, {created_at, [], [integer_to_list(CreatedAt)]}],
	EleSubject = {subject, [], [with_cdata(Name)]},
	EleFiles = {files, [], [with_cdata(files_name(Files))]},
	{'sphinx:document', [{id, integer_to_list(ID)}], IDSet ++ OtherAttrs ++ [EleSubject, EleFiles, ?CR]}.

schema() ->
	HashIds = [attr_int32("hash1"), attr_int32("hash2"), attr_int32("hash3"),
		attr_int32("hash4"), attr_int32("hash5")],
	Other = [attr_int32("query"), attr("created_at", "timestamp")],
	{'sphinx:schema', [field("subject"), field("files")] ++ HashIds ++ Other}.

field(Name) ->
	{'sphinx:field', [{name, [Name]}], []}.

attr(Name, Type) ->
	{'sphinx:attr', [{name, [Name]}, {type, [Type]}], []}.

attr_int32(Name) ->
	attr(Name, "int", "32").

attr(Name, Type, Bits) ->
	{'sphinx:attr', [{name, [Name]}, {type, [Type]}, {bits, [Bits]}], []}.

idset(Hash) ->
	{H1, H2, H3, H4, H5} = sphinx_id:fromhash(Hash),
	[{hash1, [], [integer_to_list(H1)]}, 
	{hash2, [], [integer_to_list(H2)]},
	{hash3, [], [integer_to_list(H3)]},
	{hash4, [], [integer_to_list(H4)]},
	{hash5, [], [integer_to_list(H5)]}].

files_name(Files) ->
	lists:foldl(fun({Name, _}, Acc) ->
		FName = filter_name(Name),
		case length(Acc) > 0 of
			true when length(FName) > 0 -> Acc ++ " " ++ FName;
			true -> Acc;
			false -> FName
		end
	end, [], Files).

filter_name(Name) ->
	case lists:prefix("_____padding_file", Name) of
		true -> [];
		false -> Name
	end.

with_cdata(Text) ->
	"<![CDATA[" ++ Text ++ "]]>".

%%
test_utf8() ->
	E = element("33FB6D00DD5E363653235449527EC1DC9959FCAB", "名字",
		[{"文件1", 1}, {"文件2", 2}], 1, 0, 0),
	write_xml("sphinx.xml", [E]).

test() ->
	E = element("33FB6D00DD5E363653235449527EC1DC9959FCAB", "name<abc>",
		[{"f1", 1}, {"f2<def>/aaa/def/\"'", 2}], 1, 0, 0),
	write_xml("sphinx.xml", [E]).
