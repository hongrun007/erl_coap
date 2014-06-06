-module(coap_client).
-export([get/2,get/3,get/4,put/3,delete/3]).
-include("coap.hel").
%-define(MAX_ID, 65536).
%-define(TOKEN_LENGTH, 8).
%-define(PORT, 5683).
%-define(TMP_PORT, 6666).
%-define(COAP_GET, 1).
%-define(COAP_POST, 2).
%-define(COAP_PUT, 3).
%-define(COAP_DELETE, 4).

% Public API
%% @spec get(Host::string(), URI::string(), Para::list())
%% @type Host::string() = "10.0.0.1" | "192.168.1.1" | ...
%% @type Para::list() = [options()]
%% @type options() = {urihost, Host::string()} | {uriport, string()} | {uriquery, string()} | {value, string()}
get(Host, URI, Para) ->
	Token = make_token(),
	ID = make_message_id(),
	{ok, PDU} = pdu:make_pdu(0, ?COAP_GET, Token, ID, URI),
	{ok, Newpdu} = getpara(PDU, Para),
	{ok, Destaddr} = inet_parse:address(Host),
	case gen_udp:open(?TMP_PORT, [binary, inet, {active, false}]) of
		{ok, Socket} ->
			gen_udp:send(Socket, Destaddr, ?PORT, Newpdu),
			{_, T1, T2} = now(),
			Res =  case gen_udp:recv(Socket, 0, 6000) of
				{ok, {Destaddr, ?PORT, Packet}} ->
					{_, T3, T4} = now(),
					io:format("Round trip time: ~p seconds\n", [((T3*1000000+T4)-(T1*1000000-T2))/1000000]),
					{ok, Ver, Type, Tkl, Code, MID} = pdu:get_header(Packet),
					io:format("~p~n", [{ok, Ver, Type, Tkl, Code, MID}]),
					{ok, Content} = pdu:get_content(Packet),
					io:format("value is: ~p~n", [Content]);
				{error, Reason} ->
					{error, Reason}
			end,
			gen_udp:close(Socket),
			Res;
		{error, Reason} ->
			{error, Reason}
	end.
	
put(Host, URI, Para) ->
	Token = make_token(),
	ID = make_message_id(),
	{ok, PDU} = pdu:make_pdu(0, ?COAP_PUT, Token, ID, URI),
	{ok, Newpdu} = getpara(PDU, Para),
	{ok, Destaddr} = inet_parse:address(Host),
	case gen_udp:open(?TMP_PORT, [binary, inet, {active, false}]) of
		{ok, Socket} ->
			gen_udp:send(Socket, Destaddr, ?PORT, Newpdu),
			Res =  case gen_udp:recv(Socket, 0, 6000) of
				{ok, {Destaddr, ?PORT, Packet}} ->
					{ok, Ver, Type, Tkl, Code, MID} = pdu:get_header(Packet),
					io:format("~p~n", [{ok, Ver, Type, Tkl, Code, MID}]),
					{ok, Content} = pdu:get_content(Packet),
					io:format("value is: ~p~n", [Content]);
				{error, Reason} ->
					{error, Reason}
			end,
			gen_udp:close(Socket),
			Res;
		{error, Reason} ->
			{error, Reason}
	end.

delete(Host, URI, Para) ->
	Token = make_token(),
	ID = make_message_id(),
	{ok, PDU} = pdu:make_pdu(0, ?COAP_DELETE, Token, ID, URI),
	{ok, Newpdu} = getpara(PDU, Para),
	{ok, Destaddr} = inet_parse:address(Host),
	case gen_udp:open(?TMP_PORT, [binary, inet, {active, false}]) of
		{ok, Socket} ->
			gen_udp:send(Socket, Destaddr, ?PORT, Newpdu),
			Res =  case gen_udp:recv(Socket, 0, 6000) of
				{ok, {Destaddr, ?PORT, Packet}} ->
					{ok, Ver, Type, Tkl, Code, MID} = pdu:get_header(Packet),
					io:format("~p~n", [{ok, Ver, Type, Tkl, Code, MID}]),
					{ok, Content} = pdu:get_content(Packet),
					io:format("value is: ~p~n", [Content]);
				{error, Reason} ->
					{error, Reason}
			end,
			gen_udp:close(Socket),
			Res;
		{error, Reason} ->
			{error, Reason}
	end.

get(Host, URI) ->
    Token = make_token(),
    ID = make_message_id(),
    {ok, PDU} = pdu:make_pdu(0, ?COAP_GET, Token, ID, URI),						%type,method,token,id,URI; method:COAP_CODE_GET
    {ok, Address} = inet_parse:address(Host),
    case gen_udp:open(?TMP_PORT, [binary, inet, {active, false}]) of
        {ok, Socket} ->
            gen_udp:send(Socket, Address, ?PORT, PDU),
            Res = case gen_udp:recv(Socket, 0, 3000) of					%timeout 3000 ms
                {ok, {Address, ?PORT, Packet}} ->
                    pdu:get_content(Packet);
                {error, Reason} ->
                    {error, Reason}
            end,
            gen_udp:close(Socket),
            Res;
         {error, Reason} ->
             {error, Reason}
    end.

get(Host, URI, URI_HOST, URI_PORT) ->
	Token = make_token(),
	ID = make_message_id(),
	{ok, PDU} = pdu:make_pdu(0, ?COAP_GET, Token, ID, URI),
	{ok, PDU1} = pdu:add_option(PDU, ?COAP_OPTION_URI_HOST, length(URI_HOST), URI_HOST),
	{ok, PDU2} = pdu:add_option(PDU1, ?COAP_OPTION_URI_PORT, length(URI_PORT), URI_PORT),
	{ok, Address} = inet_parse:address(Host),
	case gen_udp:open(?TMP_PORT, [binary, inet, {active, false}]) of
		{ok, Socket} ->
			gen_udp:send(Socket, Address, ?PORT, PDU2),
			Res = case gen_udp:recv(Socket, 0, 7000) of
			{ok, {Address, ?PORT, Packet}} ->
				pdu:get_header(Packet);
			{error, Reason} ->
				{error, Reason}
			end,
			gen_udp:close(Socket),
			Res;
		{error, Reason} ->
			{error, Reason}
	end.

%put(Host, URI, Val) ->
%	Token = make_token(),
%	ID = make_message_id(),
%	{ok,PDU} = pdu:make_pdu(0, ?COAP_PUT, Token, ID, URI),						%2 -> COAP_CODE_PUT
%	{ok,Address} = inet_parse:address(Host),
%	{ok,NewPDU} = pdu:add_payload(PDU, Val, lists:flatlength(Val)),
%	case gen_udp:open(?TMP_PORT, [binary, inet, {active, false}]) of
%		{ok,Socket} ->
%			gen_udp:send(Socket, Address, ?PORT, NewPDU),
%			Res = case gen_udp:recv(Socket, 0, 3000) of					%timeout 3000 ms
%				{ok, {Address, ?PORT, Packet}} ->
%					pdu:get_header(Packet);
%				{error, Reason} ->
%					{error, Reason}
%			end,
%			gen_udp:close(Socket),
%			Res;
%		{error, Reason} ->
%			{error,Reason}
%	end.

%delete(Host, URI) ->
%	Token = make_token(),
%	ID = make_message_id(),
%	{ok,PDU} = pdu:make_pdu(0, ?COAP_DELETE, Token, ID, URI),
%	{ok,Address} = inet_parse:address(Host),
%	case gen_udp:open(?TMP_PORT, [binary, inet, {active, false}]) of
%		{ok,Socket} ->
%			gen_udp:send(Socket, Address, ?PORT, PDU),
%			Res = case gen_udp:recv(Socket, 0, 3000) of
%				{ok, {Address, ?PORT, Packet}} ->
%					pdu:get_header(Packet);
%				{error, Reason} ->
%					{error, Reason}
%			end,
%			gen_udp:close(Socket),
%			Res;
%		{error, Reason} ->
%			{error,Reason}
%	end.

%subscribe(Host, URI) ->
%	Token = make_token(),
%	ID = make_message_id(),
%	{ok,PDU} = pdu:make_pdu(16#10, ?COAP_GET, Token, ID, URI),
%	{ok,Address} = inet_parse:address(Host),
%	{ok,NewPDU} = pdu:add_option(PDU, 6, 0, ""),						%COAP_OPTION_OBSERVE=6, length=0 , value=empty
%	Pid = spawn(fun recv_subscribe/0),
%	register(sub, Pid),
%	io:format("Parent PID is: ~p~n",[self()]),
%	Pid ! { self(), Address, NewPDU }.
%	loop().
%	recv_subscribe(self(), Address, NewPDU).
%	case gen_udp:open(0, [binary, inet, {active, true}]) of			%use random UDP port number
%		{ok,Socket} ->
%			Pid = spawn(fun recv_subscribe(Socket));
%			gen_udp:send(Socket, Address, ?PORT, NewPDU),
%			Res = case gen_udp:recv(Socket, 0, 3000) of
%				{ok, {Address, ?PORT, Packet}} ->
%					pdu:get_header(Packet);
%				{error, Reason} ->
%					{error, Reason}
%			end,
%			gen_udp:close(Socket),
%			Res;
%		{error, Reason} ->
%			{error,Reason}
%	end.

% Private API

make_token() ->
    make_token(?TOKEN_LENGTH, []).

make_token(Remaining, Acc) when Remaining == 0 ->
    Acc;
make_token(Remaining, Acc) ->
    make_token(Remaining - 1, [random:uniform(256 - 1)|Acc]).

make_message_id() ->
    random:uniform(?MAX_ID) - 1.

recv_subscribe() ->
	receive
		{From, HostAddress, NewPDU} ->
			io:format("i am in fun recv_subscribe~n"),
			case gen_udp:open(0, [binary, inet, {active, true}]) of         %use random UDP port number
			{ok,Socket} ->
				gen_udp:send(Socket, HostAddress, ?PORT, NewPDU),
				io:format("i have sent the packet~n"),
				recv_subscribe_loop(Socket, HostAddress, From);
			{error, Reason} ->
				io:format("{error, ~p}~n", [Reason])
			end
	end.

recv_subscribe_loop(Socket, Host, From) ->
	io:format("i am waitting for packet~n"),
	receive
		{udp, Socket, Host, ?PORT, Bin} ->
			{ok, Content} = pdu:get_content(Bin),
			{ok, Ver, Type, TKL, Code, MID} = pdu:get_header(Bin),
			io:format("{ok, ~p}~n", [Content]),
			io:format("Recever packet with version: ~p~nType: ~p~nTKL:~p~nCode:~p~nMessageID:~p~n", [Ver,Type,TKL,Code,MID]),
%			{error, Content} = pdu:get_content(Bin),
			From ! {self(), {ok, Content}},
			recv_subscribe_loop(Socket, Host, From);
		{error, Reason} ->
			From ! {self(), {error, Reason}},
			io:format("{recv_subscribe_loop get error, ~p}~n", [Reason]),
			recv_subscribe_loop(Socket, Host, From);
		{stop, subscribe} ->
			stop,
			gen_udp:close(Socket),
			io:format("sub loop is stoped.~n")
	after 5000 ->
		From ! {self(), {error, no_message}},
		recv_subscribe_loop(Socket, Host, From)
	end.

loop() ->
	receive
		{Pid, {ok, Content}} ->
			io:format("{ok, ~p}~n", [Content]),
			loop();
		{Pid, {error, Content}} ->
			io:format("{error, ~p}~n", [Content]),
			loop();
		Other ->
			io:format("{error, recvother}~n"),
			loop()
	end.

getpara(PDU, []) ->
	{ok, PDU};
getpara(PDU, Allpara) ->
	[H|L] = Allpara,
	{ok, Newpdu} = case H of
		{urihost, Val} ->
			pdu:add_option(PDU, ?COAP_OPTION_URI_HOST, length(Val), Val);
		{uriport, Val} ->
			pdu:add_option(PDU, ?COAP_OPTION_URI_PORT, length(Val), Val);
		{uriquery, Val} ->
			pdu:add_option(PDU, ?COAP_OPTION_URI_QUERY, length(Val), Val);
		{value, Val} ->
			pdu:add_payload(PDU, Val, length(Val))
	end,
	getpara(Newpdu, L).
