-module(ssp_session).
-include_lib("stdlib/include/ms_transform.hrl").
-include("./messages.hrl").

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3
]).

-record(state, {
    registry :: ets:tab() %% {Pid :: pid(), UserName :: binary(), Token :: binary()}
}).

- record(client, {
    token :: binary(),
    username :: binary(),
    pid :: pid(),
    timestamp :: integer()
}).

-export([
    register_ssp_client/3,
    ssp_client_invite/5,
    ssp_client_disconnected/1
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{
        registry = ets:new(?MODULE, [ordered_set, {keypos,#client.token}, named_table])
    }}.

register_ssp_client(Pid, Username, Password) ->
    gen_server:call(?MODULE, {register_ssp_client, Pid, Username, Password}, infinity).

ssp_client_invite(Token, From, To, Ref, Invite) ->
    gen_server:call(?MODULE, {ssp_client_invite, Token, From, To, Ref, Invite}, infinity).

ssp_client_disconnected(_Pid) ->
    ok.

handle_call({register_ssp_client, Pid, Username, _Password}, _From, #state{registry = Registry} = State) ->
    %check if user already exists
    % TODO: MUTEX required for ets
    Token = list_to_binary(uuid:to_string(uuid:uuid4())),
    ets:select_delete(Registry, ets:fun2ms(fun(#client{username=U}) when U == Username -> true end)),

    % create new
    true = ets:insert_new(Registry,
        #client{
            token = Token,
            username = Username,
            pid = Pid,
            timestamp = erlang:system_time()
        }
    ),
    {reply, {ok, Token}, State};

handle_call({ssp_client_invite, Token, From, To, Ref, Invite}, _From, #state{registry = Registry} = State) ->
    %check if user already exists
    io:format("handling ssp_client_invite ~n"),
    case ets:select(Registry, ets:fun2ms(fun(N = #client{token=T}) when T == Token -> N end)) of
        [#client{token = Token, username = From} = _Client] ->
            io:format("found peer~n"),
            Ret = handle_invite(From, To, Ref, Invite, Registry),
            {reply, Ret, State};
        [H|T] ->
            io:format("multiple session~p~n", [T]),
            {reply, {error, multiple_session}, State};
        _ ->
            io:format("invalid session~n"),
            {reply, {error, invalid_session}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_invite(From, To, Ref, Invite, Registry) ->
    io:format("handle_invite ~p~n", [Invite]),
    case ets:select(Registry, ets:fun2ms(fun(N = #client{username=T}) when T == To -> N end)) of
        [#client{pid = PID, token = _Token, username = To} = _Client] ->
            io:format("found peer ~p~n", [To]),
            PID ! {ssp_session, send_message, From, Invite},
            ok;
        [_H|_T] ->
            {error, multiple_session_for_client};
        _ ->
            {error, client_not_found}
    end.
