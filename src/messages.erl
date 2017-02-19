-module(messages).

-include("./message_defs.hrl").

-export([parse_invite/1, cook_invite/6, cook_invited/3]).


parse_invite(#{<<"payload">> := #{<<"from">> := From, <<"to">> := To,
            <<"media_attribute">> := MediaAttribute} = InvitePayload, <<"uuid">> := UUID, <<"token">> := Token} = _Invite) ->
    {ok, From, To, MediaAttribute, UUID, Token};

parse_invite(_) ->
    error.

cook_invite(From, To, MediaAttribute, UUID, Token, SessionID) ->
    #{payload => #{from => From, to => To,
    call_session => SessionID,
    media_attributes => MediaAttribute},
    uuid => UUID, token => Token, message => invite}.

cook_invited(To, UUID, SessionID) ->
    #{payload => #{to => To,
    call_session => SessionID},
    ref => UUID, message => invited}.