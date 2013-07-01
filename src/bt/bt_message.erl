%%
%% bt_message.erl
%% Kevin Lynx
%% 06.24.2013
%%
-module(bt_message).
-export([decode/1,
		 decode_handshake/1,
		 encode_handshake/2,
		 decode_metadata_msg/2,
		 encode_ext_handshake/1,
		 encode_metadata_req/2,
		 encode_metadata_reject/1,
		 decode_metadata_id/1]).
-compile(export_all).
-define(MSG_EXTEND, 20).
-define(MSG_EXT_HANDSHAKE, 0).
-define(PROTO_NAME, <<"BitTorrent protocol">>).

encode_handshake(Hash, PeerID) when is_binary(Hash), is_binary(PeerID) ->
	% to support extend message, ext[5] |= 0x10
	Ext = <<0, 0, 0, 0, 0, 16, 0, 0>>, 
	<<19:8, ?PROTO_NAME/binary, Ext/binary, Hash/binary, PeerID/binary>>.

decode_handshake(Bin) when is_binary(Bin) ->
	<<PLen:8, R/binary>> = Bin,
	<<PName:PLen/binary, Ext:8/binary, Hash:20/binary, PeerID:20/binary, Rest/binary>> = R,
	case PName == ?PROTO_NAME of
		true ->
			{Ext, Hash, PeerID, Rest};
		false ->
			{error, Bin}
	end.

% {R, not_completed}
% {R, {extend, handkshake, Dict}}
% {R, {extend, ExtMsgID, Dict}}
% {R, not_implemented}
decode(Bin) when is_binary(Bin) ->
	<<Size:32, Rest/binary>> = Bin,
	case byte_size(Rest) < Size of
		true ->
			{Bin, not_completed};
		false ->
			decode(Size, Rest)
	end.

decode(0, <<>>) -> % keep-alive
	{<<>>, not_implemented};

decode(Size, <<Type:8, Rest/binary>>) ->
	% (whole size) - (message type)
	BodySize = Size - 1,
	case Type of
		?MSG_EXTEND ->
			{Code, Dict, BodyR, R} = decode_extend(BodySize, Rest),
			{R, {extend, Code, Dict, BodyR}};
		0 ->
			{Rest, not_implemented};
		_ ->
			<<_:BodySize/binary, R/binary>> = Rest,
			{R, not_implemented}
	end.

decode_extend(Size, Bin) ->
	% exclude the extend message id
	DictSize = Size - 1,
	<<ID:8, Body:DictSize/binary, Rest/binary>> = Bin,
	case ID of
		?MSG_EXT_HANDSHAKE ->
			{{dict, D}, R} = bencode:dec(Body),
			{handshake, D, R, Rest};
		_ -> 
			{{dict, D}, R} = bencode:dec(Body),
			% may has extra data, e.g, `data' message in ut_metadata
			{ID, D, R, Rest}
	end.

decode_metadata_id(Dict) ->
	{ok, {dict, M}} = dict:find(<<"m">>, Dict),
	case dict:find(<<"ut_metadata">>, M) of
		{ok, ID} ->
			ID;
		_ ->
			not_support
	end.

% should check id match first
decode_metadata_msg(Dict, Bin) ->
	{ok, Type} = dict:find(<<"msg_type">>, Dict),
	{ok, Piece} = dict:find(<<"piece">>, Dict),
	case Type of
		0 -> % request
			Bin = <<>>,
			{request, Piece};
		1 -> % data
			% total size the total size for the torrent metadata, not the size of this piece
			{ok, Size} = dict:find(<<"total_size">>, Dict),
			{data, Piece, Size, Bin};
		2 -> % reject
			Bin = <<>>,
			{reject, Piece}
	end.

encode_ext_msg(MsgID, Dict) ->	
	Body = bencode:encode(Dict),
	Len = byte_size(Body) + 2,
	<<Len:32, 20:8, MsgID:8, Body/binary>>.

encode_ext_handshake(MetaMsgID) ->
	M = {dict, dict:from_list([{<<"ut_metadata">>, MetaMsgID}])},
	Dict = {dict, dict:from_list([{<<"m">>, M}])},
	encode_ext_msg(0, Dict).

encode_metadata_req(MsgID, Piece) ->
	Dict = {dict, dict:from_list([{<<"msg_type">>, 0}, {<<"piece">>, Piece}])},
	encode_ext_msg(MsgID, Dict).

encode_metadata_reject(MsgID) ->
	Dict = {dict, dict:from_list([{<<"msg_type">>, 2}, {<<"piece">>, 0}])},
	encode_ext_msg(MsgID, Dict).

