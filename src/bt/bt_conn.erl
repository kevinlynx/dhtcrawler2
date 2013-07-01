
-module(bt_conn).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start/4, 
		 stop/1]).
-export([test/0]).
-record(state, {data, sock, hash, req = 0, 
				msgid = 0, metasize = 0, metainfo = <<>>,
				support_utdata = false}).
-define(UT_METADATA_MSGID, 1).

start(IP, Port, Hash, Self) ->
	gen_server:start_link(?MODULE, [IP, Port, Hash, Self], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([IP, Port, Hash, Self]) ->
	{ok, {IP, Port, Hash, Self}, 0}.

terminate(_, State) when is_tuple(State) ->
	{ok, State};

terminate(_, State) ->
	#state{sock = Sock} = State,
	gen_tcp:close(Sock),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({tcp, Sock, Data}, State) ->
	#state{data = Old, req = Req} = State,
	FullData = <<Old/binary, Data/binary>>,
	{NewReq, NewData, Msgs} = parse_message(Req, FullData, []),
	inet:setopts(Sock, [{active,once}]),
	NewState = process_message(State#state{data = NewData, req = NewReq}, Msgs),
	{noreply, NewState};

handle_info(timeout, {IP, Port, Hash, Self}) ->
	Sock = case gen_tcp:connect(IP, Port, [binary, {active, once}]) of
		{ok, S} -> S;
		_ -> nil
	end,
	State = #state{data = <<>>, sock = Sock, hash = Hash, req = 1},
	Handshake = bt_message:encode_handshake(Hash, Self),
	gen_tcp:send(Sock, Handshake),
	ExtHandshake = bt_message:encode_ext_handshake(?UT_METADATA_MSGID),
	gen_tcp:send(Sock, ExtHandshake),
	{noreply, State};

handle_info(_, State) ->
	{noreply, State}.

handle_call(_, _From, State) ->
	{noreply, State}.

parse_message(1, Bin, Msgs) ->
	{_, _, _, Rest} = bt_message:decode_handshake(Bin),
	parse_message(2, Rest, Msgs);

parse_message(Req, <<>>, Msgs) ->
	{Req, <<>>, lists:reverse(Msgs)};

parse_message(Req, Bin, Msgs) ->
	case bt_message:decode(Bin) of
		{Rest, not_completed} -> 
			{Req, Rest, Msgs};
		{Rest, Msg} ->
			parse_message(Req + 1, Rest, [Msg|Msgs])
	end.

process_message(State, []) ->
	State;

process_message(State, [Msg|Rest]) ->
	NewState = process_message(State, Msg),
	process_message(NewState, Rest);

process_message(State, not_implemented) ->
	State;

process_message(State, {extend, handshake, Dict, _}) ->
	{ok, {dict, M}} = dict:find(<<"m">>, Dict),
	{ok, ID} = dict:find(<<"ut_metadata">>, M),
	{ok, Size} = dict:find(<<"metadata_size">>, Dict),
	% request the metainfo
	start_req_metainfo(State, ID),
	State#state{msgid = ID, metasize = Size};

process_message(State, {extend, MsgID, Dict, Body}) ->
	?UT_METADATA_MSGID = MsgID,
	{ok, FP} = file:open("meta.torrent", [append]),
	file:write(FP, Body),
	file:close(FP),
	MetaMsg = bt_message:decode_metadata_msg(Dict, Body),
	process_meta_message(State, MetaMsg).

% request, reject right now
process_meta_message(State, {request, _}) ->
	#state{sock = Sock, msgid = MsgID} = State,
	Msg = bt_message:encode_metadata_reject(MsgID),
	gen_tcp:send(Sock, Msg),
	State;

% data
process_meta_message(State, {data, Piece, Size, Data}) ->
	#state{metainfo = OldInfo, metasize = MetaSize, sock = Sock, msgid = MsgID} = State,
	Size = MetaSize,
	NewInfo = <<OldInfo/binary, Data/binary>>,
	case byte_size(NewInfo) >= MetaSize of
		true ->
			% load metainfo done, TODO: notify
			io:format("download metainfo done ~p ~n", [Size]),
			file:write_file("meta.torrent", NewInfo),
			ok;
		false ->
			% request next piece
			ReqMsg = bt_message:encode_metadata_req(MsgID, Piece + 1),
			gen_tcp:send(Sock, ReqMsg),
			ok
	end,
	State#state{metainfo = NewInfo};
	
% reject, ignore right now
process_meta_message(State, {reject, _}) ->
	State.

start_req_metainfo(State, ID) ->
	#state{sock = Sock} = State,
	ReqMsg = bt_message:encode_metadata_req(ID, 0),
	gen_tcp:send(Sock, ReqMsg).

%%%
test() ->
	I = bep9:random(),
	Peer = <<I:160>>,
	%Hash = <<200,126,2,108,203,24,198,144,173,99,133,8,141,160,119,166,176,58,126,169>>,
	Hash = <<77,16,191,99,137,133,31,179,255,232,239,14,116,98,74,114,233,232,39,248>>,
	%Hash = <<252,152,147,123,241,68,124,54,123,130,135,101,215,57,9, 59,102,111,53,209>>,
	start({127, 0, 0, 1}, 6881, Hash, Peer).

