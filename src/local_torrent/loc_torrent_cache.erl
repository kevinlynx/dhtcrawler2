%%
%% loc_torrent_cache.erl
%% Kevin Lynx
%% 07.05.2013
%%
-module(loc_torrent_cache).
-include("vlog.hrl").
-export([save/3, load/2]).

save(Conn, MagHash, Content) when is_list(MagHash), length(MagHash) == 40 ->
	SaveDB = config:get(save_to_db, false),
	if SaveDB -> db_loc_torrent:save(Conn, MagHash, Content); 
		true -> ok end,
	SaveFile = config:get(save_to_file, true),
	if SaveFile -> save_to_file(MagHash, Content);
		true -> ok end,
	ok.

load(Conn, MagHash) when is_list(MagHash), length(MagHash) == 40 ->
	LoadDB = config:get(load_from_db, false),
	case load_from_file(MagHash) of
		not_found when LoadDB ->
			db_loc_torrent:load(Conn, MagHash);
		not_found ->
			not_found;
		Content ->
			Content
	end.

%% TODO: put these file-realted codes to another module
save_to_file(MagHash, Content) ->
	{Path, FileName} = torrent_file_name(MagHash),
	filelib:ensure_dir(Path),
	case file:write_file(FileName, Content) of
		ok -> ok;
		{error, Reason} ->
			?E(?FMT("save torrent ~s on disk failed ~p", [FileName, Reason]))
	end.
		
load_from_file(MagHash) ->
	{_, FileName} = torrent_file_name(MagHash),
	case file:read_file(FileName) of
		{ok, Content} ->
			Content;
		{error, _} ->
			not_found
	end.

% path/AA/BB/AABBxxxxx.torrent
torrent_file_name(MagHash) ->
	Path = config:get(torrent_path, "torrents/"),
	FullPath = Path ++ lists:sublist(MagHash, 1, 2) ++ "/" ++ 
		lists:sublist(MagHash, 3, 2) ++ "/",
	{FullPath, FullPath ++ MagHash ++ ".torrent"}.

