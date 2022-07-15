
-include_lib("ep_core/include/records.hrl").
-include_lib("itx/include/records.hrl").


-ifndef(APPOSM).

-define(APPOSM, ep_osm).
-define(APPOSM_ADMIN, "admin").
-define(APPOSM_RECEIVER, "osmreceiver").
-define(APPOSM_SCANUPLOADER, "osmscanuploader").
-define(APPOSM_CONTROLLER, "osmcontroller").
-define(APPOSM_QC, "osmqc").

-define(APPOSM_ANPADMIN, "anpadmin").
-define(APPOSM_EVALUATOR, "anpevaluator").
-define(APPOSM_MODERATOR, "anpmoderator").
-define(APPOSM_REVALUATOR, "anprevaluator").
-define(APPOSM_MODERATOR_REVAL, "anpmoderator_reval").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------


%
% profiles
%
-define(OSMREC(Id), itf:field(ep_osm_receiver_fields, Id)).
-define(OSMREC(Id, Field), itf:field(ep_osm_receiver_fields, Id, Field)).

-define(OSMSUP(Id), itf:field(ep_osm_scanuploader_fields, Id)).
-define(OSMSUP(Id, Field), itf:field(ep_osm_scanuploader_fields, Id, Field)).

-define(OSMEMR(Id), itf:field(ep_osm_examiner_fields, Id)).
-define(OSMEMR(Id, Field), itf:field(ep_osm_examiner_fields, Id, Field)).

-define(OSMCON(Id), itf:field(ep_osm_controller_fields, Id)).
-define(OSMCON(Id, Field), itf:field(ep_osm_controller_fields, Id, Field)).

-define(OSMCAN(Id), itf:field(ep_osm_candidate_fields, Id)).
-define(OSMCAN(Id, Field), itf:field(ep_osm_candidate_fields, Id, Field)).

-define(OSMQC(Id), itf:field(ep_osm_qc_fields, Id)).
-define(OSMQC(Id, Field), itf:field(ep_osm_qc_fields, Id, Field)).

%
% entities
%
-define(OSMCAP(Id), itf:field(ep_osm_cap_fields, Id)).
-define(OSMCAP(Id, Field), itf:field(ep_osm_cap_fields, Id, Field)).

-define(OSMBDL(Id), itf:field(ep_osm_bundle_fields, Id)).
-define(OSMBDL(Id, Field), itf:field(ep_osm_bundle_fields, Id, Field)).

-define(OSMEXM(Id), itf:field(ep_osm_exam_fields, Id)).
-define(OSMEXM(Id, Field), itf:field(ep_osm_exam_fields, Id, Field)).

-define(OSMMSC(Id), itf:field(ep_osm_mscheme_fields, Id)).
-define(OSMMSC(Id, Field), itf:field(ep_osm_mscheme_fields, Id, Field)).

-define(OSMRLS(Id), itf:field(ep_osm_mod_rules_fields, Id)).
-define(OSMRLS(Id, Field), itf:field(ep_osm_mod_rules_fields, Id, Field)).

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------


-endif.
