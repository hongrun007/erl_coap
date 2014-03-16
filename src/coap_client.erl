-module(coap_client).
-export([get/2,put/3]).
-define(MAX_ID, 65536).
-define(TOKEN_LENGTH, 8).
-define(PORT, 5683).
-define(TMP_PORT, 6666).
-define(COAP_GET, 1).
-define(COAP_POST, 2).
-define(COAP_PUT, 3).
-define(COAP_DELETE, 4).

% Public API

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

put(Host, URI, Val) ->
	Token = make_token(),
	ID = make_message_id(),
	{ok,PDU} = pdu:make_pdu(0, ?COAP_PUT, Token, ID, URI),						%2 -> COAP_CODE_PUT
	{ok,Address} = inet_parse:address(Host),
	{ok,NewPDU} = pdu:add_payload(PDU, Val, lists:flatlength(Val)),
	case gen_udp:open(?TMP_PORT, [binary, inet, {active, false}]) of
		{ok,Socket} ->
			gen_udp:send(Socket, Address, ?PORT, NewPDU),
			Res = case gen_udp:recv(Socket, 0, 3000) of					%timeout 3000 ms
				{ok, {Address, ?PORT, Packet}} ->
%					pdu:get_connect(Packet);
					{ok, sent, {Address, ?PORT}};						% 应该改成收到确认信息
				{error, Reason} ->
					{error, Reason}
			end,
			gen_udp:close(Socket),
			Res;
		{error, Reason} ->
			{error,Reason}
	end.

% Private API

make_token() ->
    make_token(?TOKEN_LENGTH, []).

make_token(Remaining, Acc) when Remaining == 0 ->
    Acc;
make_token(Remaining, Acc) ->
    make_token(Remaining - 1, [random:uniform(256 - 1)|Acc]).

make_message_id() ->
    random:uniform(?MAX_ID) - 1.
