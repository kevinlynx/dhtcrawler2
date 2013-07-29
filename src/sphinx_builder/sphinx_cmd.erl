%%
%% sphinx_cmd.erl
%% Kevin Lynx
%% 07.28.2013
%%
-module(sphinx_cmd).
-export([build_delta_index/5, merge_index/3]).
-compile(export_all).
-include("vlog.hrl").

% Index file, Delta index name
build_delta_index(IndexFile, Delta, CfgFile, MinID, MaxID) ->
	Cmd = "indexer -c " ++ CfgFile ++ " --rotate " ++ Delta,
	Res = os:cmd(Cmd),
	Dest = backup_delta_file(Delta, MinID, MaxID, IndexFile),
	?I(?FMT("command `~s' result on ~s~n" ++ Res, [Cmd, Dest])).

merge_index(Main, Delta, CfgFile) ->
	Cmd = string_util:format("indexer -c " ++ CfgFile ++ " --merge ~s ~s --rotate",
		[Main, Delta]),
	Res = os:cmd(Cmd),
	?I(?FMT("command `~s' result~n" ++ Res, [Cmd])).

backup_delta_file(Delta, MinID, MaxID, IndexFile) ->
	Path = filename:dirname(IndexFile),
	Dest = string_util:format(Path ++ "/" ++ Delta ++ "[~b-~b]" ++ ".xml",
		[MinID, MaxID]),
	file:copy(IndexFile, Dest),
	Dest.
