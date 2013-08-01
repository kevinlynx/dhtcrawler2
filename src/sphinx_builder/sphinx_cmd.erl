%%
%% sphinx_cmd.erl
%% Kevin Lynx
%% 07.28.2013
%%
-module(sphinx_cmd).
-export([build_init_index/3, build_delta_index/5, merge_index/3]).
-compile(export_all).
-include("vlog.hrl").

build_init_index(MainFile, DeltaFile, CfgFile) ->
	case filelib:is_file(MainFile) and filelib:is_file(DeltaFile) of
		true ->
			io:format("main/delta index file exists, ignore~n", []);
		false ->
			do_build_init_index(MainFile, DeltaFile, CfgFile)
	end.

do_build_init_index(MainFile, DeltaFile, CfgFile) ->
	sphinx_doc:write_test_xml(MainFile),
	sphinx_doc:write_test_xml(DeltaFile),
	Cmd = "indexer -c " ++ CfgFile ++ " --all",
	Ret = os:cmd(Cmd),
	io:format("~p~n", [Ret]).

% Index file, Delta index name
build_delta_index(IndexFile, Delta, CfgFile, MinID, MaxID) ->
	Cmd = "indexer -c " ++ CfgFile ++ " --rotate " ++ Delta,
	Res = os:cmd(Cmd),
	Success = check_cmd_success(Res),
	Dest = backup_delta_file(Delta, MinID, MaxID, IndexFile, Success),
	?I(?FMT("command `~s' result on ~s~n" ++ Res, [Cmd, Dest])).

merge_index(Main, Delta, CfgFile) ->
	Cmd = string_util:format("indexer -c " ++ CfgFile ++ " --merge ~s ~s --rotate",
		[Main, Delta]),
	Res = os:cmd(Cmd),
	?I(?FMT("command `~s' result~n" ++ Res, [Cmd])).

backup_delta_file(Delta, MinID, MaxID, IndexFile, Flag) ->
	Path = filename:dirname(IndexFile),
	Dest = string_util:format(Path ++ "/" ++ Delta ++ "[~b-~b]" ++ ".xml",
		[MinID, MaxID]),
	if not Flag -> file:copy(IndexFile, Dest); true -> skip end,
	Dest.

% too simple
check_cmd_success(Res) ->
	string:str(Res, "succesfully") > 0.
