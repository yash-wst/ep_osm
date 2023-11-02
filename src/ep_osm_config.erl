-module(ep_osm_config).
-compile(export_all).
-include("records.hrl").


%------------------------------------------------------------------------------
% start
%------------------------------------------------------------------------------

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
% is reminders via email enabled
%------------------------------------------------------------------------------

can_send_reminders_via_email() ->
	itxconfigs_cache:get2(ep_osm_exam_reminders_via_email, false).



%------------------------------------------------------------------------------
% evaluator proctoring
%------------------------------------------------------------------------------

is_evaluation_face_proctored() ->
	itxconfigs_cache:get2(ep_osm_evaluation_face_proctored, false).

evaluation_face_proctoring_interval_secs() ->
	itxconfigs_cache:get2(ep_osm_evaluation_face_proctoring_interval_secs, 120).

evaluation_face_proctor_action() ->
	itxconfigs_cache:get2(ep_osm_evaluation_face_proctor_action, "warn").



%------------------------------------------------------------------------------
% anpstate after qc completed
%------------------------------------------------------------------------------

anp_state_after_qc_completed() ->
	itxconfigs_cache:get2(anpstate_after_qc_completed, "anpstate_yettostart").

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
