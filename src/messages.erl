-module(messages).

-include("./message_defs.hrl").

-export([parse_invite/1, cook_invite/6]).


parse_invite(#{<<"payload">> := #{<<"from">> := From, <<"to">> := To,
            <<"media_attribute">> := MediaAttribute} = InvitePayload, <<"uuid">> := UUID, <<"token">> := Token} = _Invite) ->
    {ok, From, To, MediaAttribute, UUID, Token};

parse_invite(_) ->
    error.

cook_invite(From, To, MediaAttribute, UUID, Token, CallSession) ->
    #{<<"payload">> => #{<<"from">> => From, <<"to">> => To,
        <<"call_session">> => CallSession,
        <<"media_attribute">> => MediaAttribute},
        <<"uuid">> => UUID, <<"token">> => Token, <<"message">> => "invite"}.

