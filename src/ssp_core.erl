-module(ssp_core).
-include_lib("stdlib/include/ms_transform.hrl").
-include("./message_defs.hrl").

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_info/2
]).

-record(state, {
    client_registry :: ets:tab(), %% {Pid :: pid(), UserName :: binary(), Token :: binary()}
    call_registry :: ets:tab() %% {caller :: binary(), callee :: binary(), session_id:: binary}
}).

- record(client, {
    token :: binary(),
    username :: binary(),
    pid :: pid(),
    timestamp :: integer()
}).

- record(call_session, {
    session_id :: binary(),
    caller :: binary(),
    callee :: pid(),
    state :: atom(),
    timestamp :: integer()
}).

-define(INTERVAL, 60000). % One minute

-export([
    register_ssp_client/3,
    ssp_client_invite/5,
    ssp_client_disconnected/1
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, #state{
        client_registry = ets:new(ssp_core_client_registry, [ordered_set, {keypos,#client.token}, named_table]),
        call_registry = ets:new(ssp_core_call_registry, [ordered_set, {keypos,#call_session.session_id}, named_table])
    }}.

register_ssp_client(Pid, Username, Password) ->
    gen_server:call(?MODULE, {register_ssp_client, Pid, Username, Password}, infinity).

ssp_client_invite(Token, From, To, UUID, MediaAttribute) ->
    gen_server:call(?MODULE, {ssp_client_invite, Token, From, To, UUID, MediaAttribute}, infinity).

ssp_client_disconnected(_Pid) ->
    ok.

handle_call({register_ssp_client, Pid, Username, _Password}, _From,
    #state{client_registry = ClientRegistry} = State) ->
    % TODO: MUTEX required for ets
    Token = list_to_binary(uuid:to_string(uuid:uuid4())),
    update_client_registry(ClientRegistry, Username, Pid, Token),
    {reply, {ok, Token}, State};

handle_call({ssp_client_invite, Token, From, To, UUID, MediaAttribute}, _From,
    #state{client_registry = ClientRegistry, call_registry = CallRegistry} = State) ->
    %check if user already exists
    io:format("handling ssp_client_invite ~n"),
    case ets:select(ClientRegistry, ets:fun2ms(fun(N = #client{token=T}) when T == Token -> N end)) of
        [#client{pid = CallerPID, token = Token, username = From} = _Client] ->
            io:format("found peer~n"),
            Ret = handle_invite(CallerPID, From, To, UUID, MediaAttribute, ClientRegistry, CallRegistry),
            {reply, Ret, State};
        [_H | _T] ->
            {reply, {error, multiple_session}, State};
        _ ->
            io:format("invalid session~n"),
            {reply, {error, invalid_session}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(trigger, #state{call_registry = CallRegistry} = State) ->
    Current = erlang:system_time(),
    ets:select_delete(CallRegistry, ets:fun2ms(fun(#call_session{timestamp = T, state = S}) when (T + 30000) < Current, S == inviting -> true end)),
    ets:select_delete(CallRegistry, ets:fun2ms(fun(#call_session{timestamp = T, state = S}) when (T + 60000) < Current, S == calling -> true end)),
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_invite(CallerPID, From, To, UUID, MediaAttribute, ClientRegistry, CallRegistry) ->
    case ets:select(ClientRegistry, ets:fun2ms(fun(N = #client{username=T}) when T == To -> N end)) of
        [#client{pid = PID, token = Token, username = To} = _Client] ->
            io:format("found peer ~p~n", [To]),
            SessionID = list_to_binary(uuid:to_string(uuid:uuid4())),
            update_call_registry(CallRegistry, From, To, inviting, SessionID),
            Invite = messages:cook_invite(From, To, MediaAttribute, UUID, Token, SessionID),
            PID ! {ssp_core, send_message, From, Invite},
            {ok, SessionID};
        [_H|_T] ->
            {error, multiple_session_for_client};
        _ ->
            {error, client_not_found}
    end.


update_call_registry(CallRegistry, Caller, Callee, State = inviting, SessionID) ->
    % create new
    true = ets:insert_new(CallRegistry,
        #call_session{
            session_id = SessionID,
            caller = Caller,
            callee = Callee,
            state = State,
            timestamp = erlang:system_time()
        }
    );

update_call_registry(CallRegistry, Caller, Callee, State, SessionID) ->
    %check if user already exists and delete user from record
    ets:select_delete(CallRegistry, ets:fun2ms(fun(#call_session{session_id = ID}) when ID == SessionID -> true end)),
    % create new
    true = ets:insert_new(CallRegistry,
        #call_session{
            session_id = SessionID,
            caller = Caller,
            callee = Callee,
            state = State,
            timestamp = erlang:system_time()
        }
    ).

update_client_registry(ClientRegistry, Username, Pid, Token) ->
    %check if user already exists and delete user from record
    ets:select_delete(ClientRegistry, ets:fun2ms(fun(#client{username=U}) when U == Username -> true end)),

    % create new
    true = ets:insert_new(ClientRegistry,
        #client{
            token = Token,
            username = Username,
            pid = Pid,
            timestamp = erlang:system_time()
        }
    ).