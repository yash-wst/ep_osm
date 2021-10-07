-module(ep_osm_eval_view).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

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
	Links = [
		#link {id=anpevaluator, class="default", text="Evaluator", url=url(anpevaluator)},
		#link {id=anpmoderator, text="Moderator", url=url(anpmoderator)},
		#link {id=anprevaluator, text="Revaluator", url=url(anprevaluator)},
		#link {id=anpmoderator_reval, text="Reval Moderator", url=url(anpmoderator_reval)}
	],
	itl:tabs(Links, ?L2A(wf:q(role))).



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
	% layout info
	%
	Es1 = itl:section(layout:grow([
		layout:g(4, layout_student_info(TFs, Fs)),
		layout:g(4, layout_evaluator_marking(TFs, Fs, RoleFId)),
		layout:g(4, layout_page_nos(TFs, Fs, RoleFId))
	])),



	%
	% layout pages
	%
	Es2 = [
		anpcandidate:layout_answerpaper(TFs, Fs)
	],



	%
	% return
	%
	#panel {
		style="padding-top: 100px;",
		html_id="page-content-wrapper",
		body=[
			Es1,
			#p {},
			Es2
		]
	}.



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------



%
% layout - student info
%

layout_student_info(_TFs, Fs) ->
	ProfileId = ?L2A("profileidfk_" ++ wf:q(role)),
	Es = layout:get(?VIEW, fields:getfields(Fs, [
		anpseatnumber,
		anpfullname,
		anpstate,
		ProfileId
	]), [], table),

	[
		#p {class="font-weight-bold", text="Student Info"},
		Es
	].



%
% layout - evaluator marking
%

layout_evaluator_marking(TFs, Fs, RoleFId) ->
	[
		#p {class="font-weight-bold", text="Markings"},
		anpcandidate:layout_evaluator_marking_0(TFs, Fs, RoleFId)
	].


%
% layout - page numbers
%

layout_page_nos(TFs, Fs, RoleFId) ->

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
	Rows = anpcandidate:layout_page_nos_rows(CanvasDataVal, Filenames, 4),
	Table = #table {
		class="table table-bordered",
		rows=Rows
	},


	[
		#p {class="font-weight-bold", text="Pages"},
		Table
	].




%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(Event) ->
	?D(Event).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% url
%
url(Role) ->
	io_lib:format("/ep_osm_eval_view?role=~p&anptestid=~s&anpid=~s", [
		Role, wf:q(anptestid), wf:q(anpid)
	]).



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
