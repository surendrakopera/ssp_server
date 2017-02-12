-module(ssp_server_sup).
-behaviour(supervisor).

-export(
    [start_link/0]
).
-export(
    [init/1]
).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Flags = #{strategy => rest_for_one, intensity => 1, period => 10},
    Children = [
        #{
            id => ssp_connection,
            start => {ssp_connection, start_link, [8080]}
        },
        #{
            id => ssp_client_sup,
            start => {ssp_client_sup, start_link, []},
            type => supervisor
        },
        #{
            id => ssp_session,
            type => worker,
            start => {ssp_session, start_link, []},
            restart => permanent
        }
    ],
    {ok, {Flags, Children}}.
