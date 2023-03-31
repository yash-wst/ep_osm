-module(ep_osm_eval_view).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


pagejs() -> [
	fabricjs,
	"/lib/ep_osm/priv/static/js/ep_osm_2.js"
].


main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/ep_osm/priv/static/templates/html/ep_osm_eval.html"})).

title() ->
	?LN("Evaluation View").

heading() ->
	?LN("Evaluation View").



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------

access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_CONTROLLER) -> true;
access(_, _) -> false.


%------------------------------------------------------------------------------
% tabs
%------------------------------------------------------------------------------

nav() ->
	[
		#link {id=anpcandidate, text="Candidate", url=url(anpcandidate)},
		#link {id=anpevaluator, text="Evaluator Markings", url=url(anpevaluator)},
		#link {id=anpmoderator, text="Moderator Markings", url=url(anpmoderator)},
		#link {id=anprevaluator, text="Revaluator Markings", url=url(anprevaluator)},
		#link {id=anpmoderator_reval, text="Reval Moderator Markings", url=url(anpmoderator_reval)}
	].



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------

layout() ->

	%
	% init
	%
	TestId = wf:q(anptestid),
	CandidateId = wf:q(anpid),
	TFs = anptests:get(TestId),
	Fs = anpcandidates:get(anpcandidates:db(TestId), CandidateId),
	RoleFId = ?L2A(wf:q(role)),


	%
	% init width and height
	%
	case itxconfigs_cache:get2(anpcandidate_review_module, "ep_osm_eval") of
		"anpcandidate_review" ->
			wf:wire("ANP.BG_WIDTH = 900; ANP.BG_HEIGHT = 1800;");
		_ ->
			skip
	end,


	%
	% layout
	%
	itl:wire_script("ANP.disable_selection();"),
	Es = ?AKIT({layout, card_list_group, [
		layout_student_info(TFs, Fs),
		layout:grow([
			layout_evaluator_marking(TFs, Fs, RoleFId),
			layout_page_nos(TFs, Fs, RoleFId)
		]),
		layout_proctor_photos(Fs, RoleFId),
		anpcandidate_answerpaper:layout_answerpaper(TFs, Fs)
	]}),
	akit_fullpage:layout(Es, nav()).



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------



%
% layout - student info
%

layout_student_info(_TFs, Fs) ->
	ProfileId = ?L2A("profileidfk_" ++ wf:q(role)),
	MarkingId = ?L2A("anpcanvas_" ++ wf:q(role)),
	Es = itl:get(?VIEW, fields:getfields(Fs, [
		anpseatnumber,
		anpfullname,
		anpstate,
		ProfileId,
		MarkingId
	]), noevent, table),

	[
		#p {class="font-weight-bold", text="Student Info"},
		Es
	].



%
% layout - evaluator marking
%

layout_evaluator_marking(TFs, Fs, RoleFId) ->
	layout_evaluator_marking(TFs, Fs, RoleFId, itf:val(TFs, anptesttype)).

layout_evaluator_marking(TFs, Fs, RoleFId, "anptesttype_thesis") ->
	%
	% get form
	%
	FormId = itf:val(TFs, anptesttype_thesis_formid),
	{ok, FormDoc} = ep_core_dynamic_form_api:get(FormId),
	FormRec = ep_core_dynamic_form_helper:form_rec(FormDoc),
	CandidateDocId = itf:val(Fs, '_id'),
	ProfileIdFId = ?L2A(itx:format("profileidfk_~p", [RoleFId])),
	ProfileId = itf:val(Fs, ProfileIdFId),


	%
	% layout
	%
	AppDoc = ep_osm_candidate_api:getdoc_thesis_report(
		FormId, CandidateDocId, ProfileId
	),
	ep_core_application_layout:layout(AppDoc, FormRec, ?REVIEW);

layout_evaluator_marking(TFs, Fs, RoleFId, _) ->
	Es = [
		#p {class="font-weight-bold", text="Markings"},
		anpcandidate:layout_evaluator_marking_0(TFs, Fs, RoleFId)
	],
	layout:g(4, Es).


%
% layout - page numbers
%
layout_page_nos(TFs, Fs, RoleFId) ->
	layout_page_nos(TFs, Fs, RoleFId, itf:val(TFs, anptesttype)).

layout_page_nos(_TFs, _Fs, _RoleFId, "anptesttype_thesis") ->
	[];
layout_page_nos(TFs, Fs, RoleFId, _) ->

	%
	% init
	%
	ImgUrls = anpcandidate:get_image_urls(fields:getuivalue(TFs, aws_s3_dir), TFs, Fs),
	Filenames = lists:map(fun(ImgUrl) ->
		anpcandidate:get_aname_from_imgurl(ImgUrl)
	end, ImgUrls),


	%
	% build row
	%
	CanvasDataVal = fields:getuivalue(Fs, helper:l2a("anpcanvas_" ++ ?A2L(RoleFId))),
	Rows = anpcandidate_pagenos:layout_page_nos_rows(CanvasDataVal, Filenames, 4),
	Table = #table {
		class="table table-bordered",
		rows=Rows
	},


	Es = [
		#p {class="font-weight-bold", text="Pages"},
		Table
	],
	layout:g(4, Es).




%
% layout proctor photos
%
layout_proctor_photos(Fs, RoleFId) ->
	
	%
	% init
	%
	AnpId = itf:val(Fs, '_id'),
	EvaluatorFId = ?L2A(itx:format("profileidfk_~p", [RoleFId])),
	EvaluatorId = itf:val(Fs, EvaluatorFId),


	%
	% urls
	%
	ImgUrls = get_proctor_image_urls(AnpId, EvaluatorId),
	Es = ep_osm_verify_images:layout_answerpaper(undefined, undefined, ImgUrls, 3),


	%
	% return
	%
	case ImgUrls of
		[] -> [
		];
		_ -> [
			#p {class="font-weight-bold", text="Evaluator Images"},
			Es
		]
	end.




%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({download, EvaluatorType}) ->
	anpcandidate:event({download, EvaluatorType});
event(Event) ->
	?D(Event).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% url
%
url(anpcandidate) ->
	io_lib:format("/anpcandidate?mode=~s&anptestid=~s&anpid=~s", [
		?VIEW, wf:q(anptestid), wf:q(anpid)
	]);
url(Role) ->
	io_lib:format("/ep_osm_eval_view?role=~p&anptestid=~s&anpid=~s", [
		Role, wf:q(anptestid), wf:q(anpid)
	]).



%
% get proctor image urls
%
get_proctor_image_urls(AnpId, EvaluatorId) ->

	%
	% init
	%
	Bucket = helper_s3:aws_s3_bucket(),
	Region = configs:get(aws_s3_region, "s3.ap-south-1.amazonaws.com"),

	%
	% get files from s3
	%
	ExpectDir = ep_osm_eval_v2_camera:get_proctor_base_url(AnpId, EvaluatorId) ++ "/",
	Files = helper_s3:list_keys(Bucket, ExpectDir),

	%
	% create urls from files
	%
	ImgUrls = lists:foldl(fun(L, Acc) ->
		Key = proplists:get_value(key, L),
		case string:to_lower(filename:extension(Key)) of
			Ext when Ext == ".jpg"; Ext == ".pdf" ->
				%
				% construct image url
				%
				ImgUrl = lists:flatten(io_lib:format("https://~s.~s/~s", [
					Bucket, Region, Key
				])),

				%
				% encode url
				%
				Acc ++ [anpcandidate:url_encode(ImgUrl)];
			_ ->
				Acc
		end
	end, [], Files),

	helper:sortasc(ImgUrls).

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
