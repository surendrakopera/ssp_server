-module(ssp_connection).
-export([
    start_link/1
]).

-export([
    init/1
]).

-record(state, {
    listener :: gen_tcp:socket()
}).

-spec start_link(inet:port_number()) -> {ok, pid()}.
start_link(PortNumber) ->
    io:format("~nstarting ~n"),
    proc_lib:start_link(?MODULE, init, [PortNumber]).


init(PortNumber) ->
    io:format("INITn"),
    {ok, ListenSocket} = gen_tcp:listen(PortNumber, [{reuseaddr, true}]),
    proc_lib:init_ack({ok, self()}),
    loop(ListenSocket).

loop(Listener) ->
    io:format("waiting for new connection~n"),
    {ok, Socket} = gen_tcp:accept(Listener),
    io:format("acceptiong and starting new process~n"),
    inet:setopts(Socket, [{active, true}, binary, {packet, 4}]),
    {ok, Session} = ssp_client_sup:start_client_process(Socket),
    io:format("session ~p~n", [Session]),
    ok = gen_tcp:controlling_process(Socket, Session),
    loop(Listener).
