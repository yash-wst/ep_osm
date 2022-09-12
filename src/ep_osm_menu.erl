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
			{dig_mm_ep_osm_qc, ?VIEW, ?LN("QC")},
			{dig_mm_ep_osm_cap, ?VIEW, ?LN("CAPS Centre")},
			{dig_mm_ep_osm_cap_admin, ?VIEW, ?LN("CAP Centre Admin")},
			{dig_mm_ep_core_billing_user, ?VIEW, ?LN("Billing User")}
		]},
		{?LN("Content"), [
			{dig_ep_osm_exam, ?VIEW, ?LN("OSM Exams")},
			{dig_ep_osm_mscheme, ?VIEW, ?LN("Marking Scheme")},
			{dig_mm_ep_osm_mod_rules, ?VIEW, ?LN("Moderation Rules")},
			{dig_mm_ep_osm_apt, ?VIEW, ?LN("Appointments")}
		]},
		{?LN("Status"), [
			{dig_ep_osm_exam_stats, ?VIEW, ?LN("Exam Status")},
			{dig_ep_osm_bundle_stats, ?VIEW, ?LN("Scanning Status")},
			{dig_ep_osm_exam_evaluation_stats, ?VIEW, ?LN("Evaluation Status")},
			{dig_ep_osm_exam_evaluator_stats, ?VIEW, ?LN("Evaluator Status")},
			{dig_ep_osm_exam_capcentre_stats, ?VIEW, ?LN("CAP Centre Status")}
		]},
		{?LN("Results"), [
			{dig_ep_osm_exam_results, ?VIEW, ?LN("OSM Results")}
		]},
		{?LN("Reports"), [
			{dig_ep_osm_exam_evaluator_report, ?VIEW, ?LN("Evaluator Report")},
			{dig_ep_osm_bundle_daily_report, ?VIEW, ?LN("Daily Status Report")},
			{dig_mm_ep_osm_bundle, ?VIEW, ?LN("Bundle Status Report")}
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
			{dig_mm_ep_osm_cap, ?VIEW, ?LN("CAPS Centre")},
			{dig_mm_ep_osm_cap_admin, ?VIEW, ?LN("CAP Centre Admin")},
			{dig_mm_ep_core_billing_user, ?VIEW, ?LN("Billing User")}
		]},
		{?LN("Content"), [
			{dig_ep_osm_exam, ?VIEW, ?LN("OSM Exams")},
			{dig_ep_osm_mscheme, ?VIEW, ?LN("Marking Scheme")},
			{dig_mm_ep_osm_mod_rules, ?VIEW, ?LN("Moderation Rules")}
		]},
		{?LN("Status"), [
			{dig_ep_osm_exam_stats, ?VIEW, ?LN("Exam Status")},
			{dig_ep_osm_bundle_stats, ?VIEW, ?LN("Scanning Status")},
			{dig_ep_osm_exam_evaluation_stats, ?VIEW, ?LN("Evaluation Status")},
			{dig_ep_osm_exam_evaluator_stats, ?VIEW, ?LN("Evaluator Status")},
			{dig_ep_osm_bundle_daily_report, ?VIEW, ?LN("Daily Scanning Status")}
		]},
		{?LN("Results"), [
			{dig_ep_osm_exam_results, ?VIEW, ?LN("OSM Results")}
		]},
		{?LN("Reports"), [
			{dig_ep_osm_exam_evaluator_report, ?VIEW, ?LN("Evaluator Report")},
			{dig_ep_osm_bundle_daily_report, ?VIEW, ?LN("Daily Status Report")},
			{dig_mm_ep_osm_bundle, ?VIEW, ?LN("Bundle Status Report")}
		]}
	];

menu(Role) when
	Role == ?APPOSM_EVALUATOR;
	Role == ?APPOSM_MODERATOR;
	Role == ?APPOSM_REVALUATOR;
	Role == ?APPOSM_MODERATOR_REVAL ->
	case itxconfigs_cache:get2(ep_osm_menu_evaluators, false) of
		true ->
			ProfileModule = ?L2A(itx:format("profile_~s", [Role])),
			[
				{topmenu, [
					{ProfileModule, bank_details, ?LN("Bank Details")},
					{dig_ep_osm_exam_evaluator_report, ?VIEW, ?LN("Evaluation Report")}
				]}
			];
		false -> [
		]
	end;



menu(?APPOSM_CONTROLLER) ->
	[
		{topmenu, [
			{dig_ep_osm_exam_stats, ?VIEW, ?LN("Exam Status")},
			{dig_ep_osm_bundle_stats, ?VIEW, ?LN("Scanning Status")},
			{dig_ep_osm_exam_evaluation_stats, ?VIEW, ?LN("Evaluation Status")},
			{dig_ep_osm_exam_evaluator_stats, ?VIEW, ?LN("Evaluator Status")},
			{dig_ep_osm_exam_verification, ?VIEW, ?LN("Verification")},
			{dig_rds_payments, ?VIEW, ?LN("Payments")}
		]}
	];

menu(?APPOSM_CAPADMIN) ->
	[
		{topmenu, [
			{dig_ep_osm_exam_evaluation_stats, ?VIEW, ?LN("Evaluation Status")},
			{dig_ep_osm_exam_capcentre_stats, ?VIEW, ?LN("Centre Status")}
		]}
	];

menu(?APPOSM_SCANUPLOADER) ->
	[
		{topmenu, [
			{dig_ep_osm_exam_pendingbundles, ?VIEW, ?LN("Pending Bundles")},
			{dig_ep_osm_exam_mybundle, ?VIEW, ?LN("My Bundles")},
			{dig_ep_osm_exam_bundle, ?VIEW, ?LN("Find Exams")}
		]}
	];

menu(?APPOSM_QC) ->
	[
		{topmenu, [
			{dig_ep_osm_exam_pendingbundles, ?VIEW, ?LN("Pending Bundles")},
			{dig_ep_osm_exam_mybundle, ?VIEW, ?LN("My Bundles")},
			{dig_ep_osm_exam_bundle, ?VIEW, ?LN("Find Exams")}
		]}
	];


menu(?APPOSM_RECEIVER) ->
	[
		{topmenu, [
			{dig_ep_osm_exam_pendingbundles, ?VIEW, ?LN("Pending Bundles")},
			{dig_ep_osm_exam_mybundle, ?VIEW, ?LN("My Bundles")},
			{dig_ep_osm_exam_bundle, ?VIEW, ?LN("Find Exams")},
			{dig_ep_osm_bundle_stats, ?VIEW, ?LN("Scanning Status")}
		]},
		{?LN("Profiles"), [
			{dig_mm_ep_osm_scanuploader, ?VIEW, ?LN("Scanner & Uploader")}
		]},
		{?LN("Reports"), [
			{dig_ep_osm_bundle_daily_report, ?VIEW, ?LN("Daily Status Report")},
			{dig_mm_ep_osm_bundle, ?VIEW, ?LN("Bundle Status Report")}
		]}
	];

menu(_) ->
	[].
