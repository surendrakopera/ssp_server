-module(ssp_session_sup).
-export([
    start_session/1
]).

-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).


-spec start_session(gen_tcp:socket()) -> {ok, pid()}.
start_session(Socket) ->
    io:format("starting session~n"),
    Rsp = supervisor:start_child(?MODULE, [Socket]),
    io:format("RSP = ~p~n", [Rsp]),
    Rsp.

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
init([]) ->
    Flags = #{strategy => simple_one_for_one, intensity => 10000, period => 10},
    Children = [
        #{
            id => ssp_session,
            start => {ssp_session, start_link, []},
            restart => temporary
        }
    ],
    {ok, {Flags, Children}}.
