-module(message_parser).

-include("./messages.hrl").

-export([parse_invite/1]).

parse_invite(#{<<"header">> := #{<<"from">> := From, <<"to">> := To,
            <<"media_attribute">> := _MediaAttribute} = _InviteHeader, <<"message_ref">> := Ref} = _Invite) ->
    {ok, From, To, Ref};

parse_invite(_) ->
    error.


