%%
%% sphinx_excerpt.erl
%% Kevin Lynx
%% 08.24.2013
%%
-module(sphinx_excerpt).
-export([build_excerpt/5, build_excerpt/3]).
-compile(export_all).

build_excerpt(Key, Docs, Index) ->
	build_excerpt(localhost, 9312, Key, Docs, Index).

build_excerpt(IP, Port, Key, Docs, Index) 
when is_binary(Key), is_binary(Index), is_list(Docs) ->
	case connect(IP, Port) of
		{ok, Sock} ->
			Ret = do_build_excerpt(Sock, Key, Docs, Index),
          	catch gen_tcp:close(Sock),
          	Ret;
		Error ->
			Error
	end.

do_build_excerpt(Sock, Key, Docs, Index) ->
	Flag = 257, % 1 | 256, allow_empty not work
	BeforeMatch = <<"<span class='highlight'>">>,
	AfterMatch = <<"</span>">>,
	ChunkSep = <<"...">>,
	Limit = 256,
	Around = 5,
	LimitPassages = 0,
	LimitWords = 0,
	StartPageId = 1,
	HtmlStripMode = <<"index">>,	
	PassageBoundary = <<"none">>,
	Commands = [{32, 0}, {32, Flag},
		{string, Index}, {string, Key}, {string, BeforeMatch},
		{string, AfterMatch}, {string, ChunkSep}, {32, Limit},
		{32, Around}, {32, LimitPassages}, {32, LimitWords},
		{32, StartPageId}, {string, HtmlStripMode}, {string, PassageBoundary}, {32, length(Docs)}] ++
		[{string, Doc} || Doc <- Docs],
  	{Bytes, Size} = giza_protocol:commands_to_bytes(Commands),
  	giza_protocol:write_number(Sock, 1, 16),
  	giza_protocol:write_number(Sock, 259, 16),
  	giza_protocol:write_number(Sock, Size, 32),
  	gen_tcp:send(Sock, Bytes),
	parse_excerpt_res(Sock, Docs).

parse_excerpt_res(Sock, Docs) ->
	{ok, <<_:16, _:16, Len:32>>} = gen_tcp:recv(Sock, 8),
    true = Len > 0,
	[read_string_res(Sock, Doc) || Doc <- Docs].

read_string_res(Sock, Doc) ->
	R = giza_protocol:read_lp_string(Sock),
	case byte_size(R) == byte_size(Doc) of 
		true -> <<>>;
		false -> R
	end.

connect(Host, Port) ->
  case gen_tcp:connect(Host, Port,
                       [binary, {packet, raw},
                        {active, false}]) of
    {ok, Sock} ->
    	{ok, _RawVersion} = gen_tcp:recv(Sock, 4),
    	giza_protocol:write_number(Sock, 1, 32),
        {ok, Sock};
    _ -> error
  end.
%
test() ->
	build_excerpt(localhost, 9312, <<"avi hi">>, [<<"hello">>, <<"hi, a a a hello avi world">>], <<"xml">>).

