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


