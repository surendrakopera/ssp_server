-module(messages).

-include("./message_defs.hrl").

-export([
    parse_invite/1,
    parse_ready/1
]).

-export([
    cook_invite/6,
    cook_invited/3,
    cook_ready/3
]).


parse_invite(#{<<"payload">> := #{<<"from">> := From, <<"to">> := To,
            <<"media_attribute">> := MediaAttribute} = _InvitePayload, <<"uuid">> := UUID, <<"token">> := _Token} = _Invite) ->
    {ok, From, To, MediaAttribute, UUID};

parse_invite(_) ->
    error.

parse_ready(#{<<"payload">> := #{<<"call_session">> := SessionID} = ReadyPayload,
    <<"uuid">> := UUID, <<"token">> := _Token, <<"ref">> := Ref} = _Ready) ->
    {ok, SessionID, UUID, Ref, ReadyPayload};

parse_ready(_) ->
    error.

cook_invite(From, To, MediaAttribute, UUID, Token, SessionID) ->
    #{payload => #{from => From, to => To,
    call_session => SessionID,
    media_attribute => MediaAttribute},
    uuid => UUID, token => Token, message => invite}.

cook_invited(To, UUID, SessionID) ->
    #{payload => #{to => To,
    call_session => SessionID},
    ref => UUID, message => invited}.

cook_ready(UUID, Ref, ReadyPayload) ->
    #{payload => ReadyPayload,
    uuid => UUID, ref => Ref, message => ready}.