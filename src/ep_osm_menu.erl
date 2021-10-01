-module(ep_osm_menu).
-compile(export_all).
-include("records.hrl").



menu(?APPOSM_ADMIN) ->
	[
		{?LN("Profiles"), [
			{profile_anpadmin, ?DBVIEW, ?LN("OSM Admin")},
			{dig_mm_profile_anpevaluator, "profile_anpevaluator", ?LN("Evaluators")},
			{dig_mm_profile_anpevaluator, "profile_anpmoderator", ?LN("Moderators")},
			{dig_mm_profile_anpevaluator, "profile_anprevaluator", ?LN("Revaluators")},
			{dig_mm_profile_anpevaluator, "profile_anpmoderator_reval", ?LN("Reval Moderator")},
			{dig_mm_ep_osm_controller, ?VIEW, ?LN("Controller")},
			{dig_mm_ep_osm_receiver, ?VIEW, ?LN("Receiver")},
			{dig_mm_ep_osm_scanuploader, ?VIEW, ?LN("Scanner & Uploader")},
			{dig_mm_ep_osm_cap, ?VIEW, ?LN("CAPS Centre")}
		]}
	];


menu(?APPOSM_OSMADMIN) ->
	[
	];

menu(?APPOSM_EVALUATOR) ->
	[
	];

menu(?APPOSM_MODERATOR) ->
	[
	];

menu(?APPOSM_REVALUATOR) ->
	[
	];

menu(?APPOSM_MODERATOR_REVAL) ->
	[
	];


menu(?APPOSM_CONTROLLER) ->
	[
		{?LN("OSM Exams"), [
			{dig_ep_osm_exam_stats, ?VIEW, ?LN("Exam Statistics")},
			{dig_ep_osm_exam_evaluation_stats, ?VIEW, ?LN("Exam Evaluation Statistics")},
			{dig_ep_osm_exam_evaluator_stats, ?VIEW, ?LN("Exam Evaluator Statistics")},
			{dig_ep_osm_bundle_stats, ?VIEW, ?LN("Scanning Statistics")},
			{dig_ep_osm_exam_verification, ?VIEW, ?LN("Exam Verification")},
			{dig_rds_payments, ?VIEW, ?LN("Payments")}
		]}
	];


menu(?APPOSM_SCANUPLOADER) ->
	[
		{?LN("OSM Exams"), [
			{dig_ep_osm_exam_bundle, ?VIEW, ?LN("Find Exams")}
		]}
	];


menu(?APPOSM_RECEIVER) ->
	[
		{?LN("OSM Exams"), [
			{dig_ep_osm_exam_bundle, ?VIEW, ?LN("Find Exams")}
		]}
	];

menu(_) ->
	[].
