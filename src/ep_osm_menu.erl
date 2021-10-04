-module(ep_osm_menu).
-compile(export_all).
-include("records.hrl").



menu(?APPOSM_ADMIN) ->
	[
		{?LN("Profiles"), [
			{profile_anpadmin, ?DBVIEW, ?LN("Admin")},
			{profile_anpstaff, ?DBVIEW, ?LN("Staff")},
			{dig_mm_profile_anpevaluator, "profile_anpevaluator", ?LN("Evaluators")},
			{dig_mm_profile_anpevaluator, "profile_anpmoderator", ?LN("Moderators")},
			{dig_mm_profile_anpevaluator, "profile_anprevaluator", ?LN("Revaluators")},
			{dig_mm_profile_anpevaluator, "profile_anpmoderator_reval", ?LN("Reval Moderator")},
			{dig_mm_ep_osm_controller, ?VIEW, ?LN("Controller")},
			{dig_mm_ep_osm_receiver, ?VIEW, ?LN("Receiver")},
			{dig_mm_ep_osm_scanuploader, ?VIEW, ?LN("Scanner & Uploader")},
			{dig_mm_ep_osm_cap, ?VIEW, ?LN("CAPS Centre")}
		]},
		{?LN("Content"), [
			{dig_ep_osm_exam, ?VIEW, ?LN("OSM Exams")},
			{dig_ep_osm_mscheme, ?VIEW, ?LN("Marking Scheme")},
			{dig_mm_ep_osm_mod_rules, ?VIEW, ?LN("Moderation Rules")}
		]},
		{?LN("Results"), [
			{dig_ep_osm_exam_results, ?VIEW, ?LN("OSM Results")}
		]},
		{?LN("Reports"), [
			{dig_ep_osm_exam_stats, ?VIEW, ?LN("Exam Status")},
			{dig_ep_osm_bundle_stats, ?VIEW, ?LN("Scanning Status")},
			{dig_ep_osm_exam_evaluation_stats, ?VIEW, ?LN("Evaluation Status")},
			{dig_ep_osm_exam_evaluator_stats, ?VIEW, ?LN("Evaluator Status")}
		]}
	];


menu(?APPOSM_ANPADMIN) ->
	[
		{?LN("Profiles"), [
			{profile_anpstaff, ?DBVIEW, ?LN("Staff")},
			{dig_mm_profile_anpevaluator, "profile_anpevaluator", ?LN("Evaluators")},
			{dig_mm_profile_anpevaluator, "profile_anpmoderator", ?LN("Moderators")},
			{dig_mm_profile_anpevaluator, "profile_anprevaluator", ?LN("Revaluators")},
			{dig_mm_profile_anpevaluator, "profile_anpmoderator_reval", ?LN("Reval Moderator")},
			{dig_mm_ep_osm_controller, ?VIEW, ?LN("Controller")},
			{dig_mm_ep_osm_receiver, ?VIEW, ?LN("Receiver")},
			{dig_mm_ep_osm_scanuploader, ?VIEW, ?LN("Scanner & Uploader")},
			{dig_mm_ep_osm_cap, ?VIEW, ?LN("CAPS Centre")}
		]},
		{?LN("Content"), [
			{dig_ep_osm_exam, ?VIEW, ?LN("OSM Exams")},
			{dig_ep_osm_mscheme, ?VIEW, ?LN("Marking Scheme")},
			{dig_mm_ep_osm_mod_rules, ?VIEW, ?LN("Moderation Rules")}
		]},
		{?LN("Results"), [
			{dig_ep_osm_exam_results, ?VIEW, ?LN("OSM Results")}
		]},
		{?LN("Reports"), [
			{dig_ep_osm_exam_stats, ?VIEW, ?LN("Exam Status")},
			{dig_ep_osm_bundle_stats, ?VIEW, ?LN("Scanning Status")},
			{dig_ep_osm_exam_evaluation_stats, ?VIEW, ?LN("Evaluation Status")},
			{dig_ep_osm_exam_evaluator_stats, ?VIEW, ?LN("Evaluator Status")}
		]}
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
