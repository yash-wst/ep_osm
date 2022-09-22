-module(ep_osm_helper).
-compile(export_all).
-include("records.hrl").
-include_lib("itx/include/records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%------------------------------------------------------------------------------
% start
%------------------------------------------------------------------------------

completed_state_of("anpevaluator") ->
	"anpstate_completed";
completed_state_of("anpmoderator") ->
	"anpstate_moderation_completed";
completed_state_of("anprevaluator") ->
	"anpstate_revaluation_completed";
completed_state_of("anpmoderator_reval") ->
	"anpstate_moderation_reval_completed";
completed_state_of(Role) ->
	throw(Role).


get_anpstate_shorthand(State) ->
	?LN(?L2A(State++"_min")).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
