%%
%% dht_monitor.erl
%% Kevin Lynx
%% 06.14.2013
%% TODO: better to use gen_server
%%
-module(dht_monitor).
-include("vlog.hrl").
-export([handle_event/2,
		 handle_torrent/3,
		 process_infohash_event/1,
		 save_to_db/3,
		 tell_more_nodes/1]).
-export([debug_dump/4,
		 debug_dump_failed/1]).
-define(QUERY_INTERVAL, 1*60*1000).

% depends on the test log, `get_peers' > `announce_peer'
handle_event(announce_peer, {_InfoHash, _IP, _BTPort}) ->
	crawler_stats:announce();

handle_event(get_peers, {InfoHash, _IP, _Port}) ->
	crawler_stats:get_peers(),
	%spawn(?MODULE, process_infohash_event, [InfoHash]);
	MagHash = dht_id:tohex(InfoHash),
	db_hash:insert(MagHash);

handle_event(startup, {MyID}) ->
	spawn(?MODULE, tell_more_nodes, [MyID]).

% since some operation will wait infinity, so spawn a new process
% NOTE: this may cause many processes, depends on database operation speed.
process_infohash_event(InfoHash) ->
	MagHash = dht_id:tohex(InfoHash),
	Wait = 60*1000,
	try 
		case torrent_index:inc_announce(MagHash, Wait) of
			true -> 
				crawler_stats:saved(false);
			false ->
				download(InfoHash)
		end
	catch
		exit:{timeout, _} -> 
			?E(?FMT("inc_announce timeout exception for ~s", [MagHash]))
	end.

tell_more_nodes(MyID) ->
	search:get_peers(MyID, dht_id:random()),
	timer:sleep(?QUERY_INTERVAL),
	tell_more_nodes(MyID). % tail recursive, be careful

download(InfoHash) ->
	MagHash = dht_id:tohex(InfoHash),
	torrent_download:download(MagHash, ?MODULE).

handle_torrent(ok, MagHash, TContent) ->
	case catch(torrent_file:parse(TContent)) of
		{'EXIT', _} ->
			?E(?FMT("parse torrent file failed ~p", [TContent]));
		{Type, Info} -> 
			save_to_db(MagHash, Type, Info)
	end,
	ok;

handle_torrent(error, _MagHash, _TContent) ->
	ok.

save_to_db(MagHash, single, {Name, Length}) ->
	torrent_index:insert(MagHash, Name, Length);

save_to_db(MagHash, multi, {Root, Files}) ->
	torrent_index:insert(MagHash, Root, 0, Files).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debug_dump_failed(MagHash) ->
	io:format("download ~s failed~n", [MagHash]),
	{ok, FP} = file:open("magnets.txt", [append]),
	io:format(FP, "~s~n", [format_magnet(MagHash)]),
	io:format(FP, "    download torrent failed [~s]~n", [get_time_string()]),
	file:close(FP).

get_time_string() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    Str = io_lib:format("~b-~2.10.0b-~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b",
    	[Year, Month, Day, Hour, Min, Sec]),
    lists:flatten(Str).

format_magnet(MagHash) ->
	"magnet:?xt=urn:btih:" ++ MagHash.

output_torrent_info(single, {Name, Length}, FP) ->
	io:format(FP, "    ~s ~s~n", [Name, torrent_file:size_string(Length)]);

output_torrent_info(multi, {Root, Files}, FP) ->
	io:format(FP, "    ~s~n", [Root]),
	[io:format(FP, "    ~s ~s~n", [Path, torrent_file:size_string(Length)]) 
		|| {Path, Length} <- Files].

debug_dump(MagHash, TContent, Type, Info) ->
	Filename = save_torrent(MagHash, TContent),
	io:format("download ~s success (~p byts), save as ~s~n", 
		[MagHash, byte_size(TContent), Filename]),
	TSize = byte_size(TContent),
	{ok, FP} = file:open("magnets.txt", [append]),
	io:format(FP, "~s~n", [format_magnet(MagHash)]),
	io:format(FP, "    download torrent success [~s] ~s (~s)~n", 
		[get_time_string(), Filename, torrent_file:size_string(TSize)]),
	output_torrent_info(Type, Info, FP),
	file:close(FP).

save_torrent(MagHash, TContent) ->
	Dir = "download/",
	filelib:ensure_dir(Dir),
	Filename = Dir ++ MagHash ++ ".torrent",
	file:write_file(Filename, TContent),
	Filename.
