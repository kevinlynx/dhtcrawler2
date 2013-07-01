%%
%% torrent_file.erl
%% Kevin Lynx
%% 06.12.2013
%% parse torrent torrent_file, it's dependent on bencode module.
%%
-module(torrent_file).
-export([parse_from_file/1, 
		 parse/1, 
		 size_brief/1,
		 size_string/1]).

parse_from_file(File) ->
	{ok, Data} = file:read_file(File),
	parse(Data).

% Format:
%   {single, {name, length}}
%	{multi, {root, [{path, length}, {path, length}]}}
parse(Content) ->
	{ok, {dict, TD}} = bencode:decode(Content),	
	{ok, {dict, Info}} = dict:find(<<"info">>, TD),
	case type(Info) of
		single -> {single, parse_single(Info)};
		multi -> {multi, parse_multi(Info)}
	end.

size_brief(SizeInBytes) ->
	BT = 1024,
	KT = BT * 1024,
	MT = KT * 1024,
	GT = MT * 1024,
	if 
		SizeInBytes < BT -> {byte, SizeInBytes};
		SizeInBytes < KT -> {kb, SizeInBytes div BT};
		SizeInBytes < MT -> {mb, SizeInBytes div KT};
		SizeInBytes < GT -> {gb, SizeInBytes div MT};
		true -> {byte, SizeInBytes}
	end.

size_string(SizeInBytes) ->
	{T, N} = size_brief(SizeInBytes),
	lists:flatten(io_lib:format("~b ~s", [N, atom_to_list(T)])).

type(Info) ->
	case dict:find(<<"files">>, Info) of 
		{ok, {list, _Files}} -> multi;
		_ -> single
	end.

parse_single(Info) ->
	Name = read_string("name", Info),
	{ok, Length} = dict:find(<<"length">>, Info),
	{Name, Length}.

parse_multi(Info) ->
	Root = read_string("name", Info),
	{ok, {list, Files}} = dict:find(<<"files">>, Info),
	FileInfo = [parse_file_item(Item) || {dict, Item} <- Files],
	{Root, FileInfo}.

parse_file_item(F) ->
	{ok, Length} = dict:find(<<"length">>, F),
	Path = read_path(F),
	{Path, Length}.

read_string(Key, Dict) ->
	% prefer utf8
	case dict:find(list_to_binary(Key++".utf-8"), Dict) of
		{ok, UTF8BS} -> 
			binary_to_list(UTF8BS);
		_ ->
			{ok, BS} = dict:find(list_to_binary(Key), Dict),
			binary_to_list(BS)
	end.

read_path(F) ->
	case dict:find(<<"path.utf-8">>, F) of
		{ok, {list, Paths}} ->
			concat_path(Paths);
		_ ->
			{ok, {list, Paths}} = dict:find(<<"path">>, F),	
			concat_path(Paths)
	end.

concat_path(Paths) ->
	AppendSlash = fun(BS, Acc) ->
		case Acc of 
			[] -> binary_to_list(BS);
			_ -> Acc ++ "/" ++ binary_to_list(BS)
		end
	end,
	lists:foldl(AppendSlash, "", Paths).

