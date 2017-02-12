-module(ssp_client_sup).
-export([
    start_client_process/1
]).

-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).


-spec start_client_process(gen_tcp:socket()) -> {ok, pid()}.
start_client_process(Socket) ->
    io:format("starting start_client_process~n"),
    Rsp = supervisor:start_child(?MODULE, [Socket]),
    io:format("started client process ~p~n", [Rsp]),
    Rsp.

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
init([]) ->
    Flags = #{strategy => simple_one_for_one, intensity => 10000, period => 10},
    Children = [
        #{
            id => ssp_client,
            start => {ssp_client, start_link, []},
            restart => temporary
        }
    ],
    {ok, {Flags, Children}}.
