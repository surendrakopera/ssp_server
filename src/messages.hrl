-include("./defines.hrl").

-record(register_packet, {
    username :: binary(),
    password :: binary()
}).

-record(ssp_invite, {
    to :: binary(),
    from :: binary()
}).

-type ssp_invite() :: ssp_invite.

-record(ssp_invite_packet, {
    token :: uuid(),
    ssp_invite :: ssp_invite()
}).