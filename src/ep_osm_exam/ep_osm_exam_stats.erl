
-module(ep_osm_exam_stats).
-compile(export_all).
-include("records.hrl").

%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

getstats_ip_state_date_evaluator(TestId, SK, EK, GroupLevel) ->

	%
	% init
	%
	ViewName = "ip_state_date_evaluator",


	%
	% query
	%
	try
		itxview:get_stats(
			anpcandidates:db(TestId), ViewName, SK, EK, GroupLevel
		)
	catch error:{badmatch,{error,not_found}} ->
		anptests:setup(TestId),
		itxview:get_stats(
			anpcandidates:db(TestId), ViewName, SK, EK, GroupLevel
		)
	end.

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
