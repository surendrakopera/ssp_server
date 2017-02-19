-module(ssp_client).
-export([
    start_link/1
]).

-behaviour(gen_statem).
-export([
    init/1,
    code_change/4,
    terminate/3,
    callback_mode/0
]).

-include("./message_defs.hrl").

-export([
    waiting_for_message/3
]).

-record(data, {
    socket :: gen_tcp:socket()
}).

callback_mode() -> state_functions.

-spec start_link(gen_tcp:socket()) -> {ok, pid()}.
start_link(Socket) ->
    io:format("starting statem~n"),
    Rsp = gen_statem:start_link(?MODULE, [Socket], []),
    io:format("started statem~n"),
    Rsp.

%% @hidden
init([Socket]) ->
    {ok, waiting_for_message, #data{
        socket = Socket
    }}.

%% @hidden
terminate(_Reason, _State, _Data) ->
    ok.

%% @hidden
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.


waiting_for_message(info, {tcp, Socket, Message}, #data{socket = Socket} = Data) ->
    Decoded = jsone:decode(Message),
    io:format("message rcvd ~n~p~n decoded message ~p ~n", [size(Message), Decoded]),
    handle_message(Decoded, Socket),
    {keep_state, Data};

waiting_for_message(info, {tcp_closed, Socket}, #data{socket = Socket}) ->
    {stop, normal};

waiting_for_message(info, {ssp_core, send_message, From, Message}, #data{socket = Socket} = Data) ->
    io:format("sending message from ~p~n", [From]),
    gen_tcp:send(Socket, jsone:encode(Message)),
    {keep_state, Data};

waiting_for_message(_, _, Data) ->
    {keep_state, Data}.

handle_message(#{<<"message">> := <<"register">>, <<"username">> := Username, <<"password">> := Password} = _Decoded, Socket) ->
    io:format("~nhandling register~n"),
    case ssp_core:register_ssp_client(self(), Username, Password) of
        {ok, Token} ->
            Message = #{ret=>true, message=>registration, token=>Token},
            gen_tcp:send(Socket, jsone:encode(Message));
        A ->
            io:format("error ~p~n", [A]),
            error
    end;

handle_message(#{<<"message">> := <<"invite">>, <<"token">> := Token} = Invite, _Socket) ->
    io:format("~n INVITE ~n ~n"),
    case messages:parse_invite(Invite) of
        {ok, From, To, MediaAttribute, UUID, Token} ->
            io:format("~n Calling SSP function ~n ~n"),
            ssp_core:ssp_client_invite(Token, From, To, UUID, MediaAttribute);
        Rsp ->
            io:format("~n recieved ~p~n", [Rsp]),
            error
    end;

handle_message(Message, _Socket) ->
    io:format("unhandleed data ~p~n", [Message]),
    ok.