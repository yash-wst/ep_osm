
-include_lib("ep_core/include/records.hrl").
-include_lib("itx/include/records.hrl").


-ifndef(APPOSM).

-define(APPOSM, ep_osm).
-define(APPOSM_ADMIN, "admin").
-define(APPOSM_RECEIVER, "osmreceiver").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------


%
% profiles
%
-define(OSMREC(Id), itf:field(ep_osm_receiver_fields, Id)).
-define(OSMREC(Id, Field), itf:field(ep_osm_receiver_fields, Id, Field)).



%
% entities
%
-define(OSMCAP(Id), itf:field(ep_osm_cap_fields, Id)).
-define(OSMCAP(Id, Field), itf:field(ep_osm_cap_fields, Id, Field)).

-define(OSMBDL(Id), itf:field(ep_osm_bundle_fields, Id)).
-define(OSMBDL(Id, Field), itf:field(ep_osm_bundle_fields, Id, Field)).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------


-endif.
