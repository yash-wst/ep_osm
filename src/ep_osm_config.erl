-module(ep_osm_config).
-compile(export_all).
-include("records.hrl").


is_qc_enabled() ->
	itxconfigs_cache:get2(ep_osm_enable_qc, false).

%------------------------------------------------------------------------------
% evaluator cannot submit until this much time has elapsed. Unit seconds.
% Default 0 seconds.
%------------------------------------------------------------------------------
get_min_time_for_evaluation() ->
	Val = itxconfigs_cache:get2(ep_osm_min_evaluation_time_in_secs, 0),
	case is_list(Val) of
		true ->
			helper:s2i(Val);
		_ ->
			Val
	end.



%------------------------------------------------------------------------------
% dtp marks can be entered only if candidate is in the following states
%------------------------------------------------------------------------------

get_dtp_marks_enabled_states() ->
	itxconfigs_cache:get2(ep_osm_exam_dtp_marks_enabled_states, [
			"anpstate_expected", "anpstate_yettostart"
	]).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
