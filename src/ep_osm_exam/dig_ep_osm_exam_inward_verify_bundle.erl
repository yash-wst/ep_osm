
-module(dig_ep_osm_exam_inward_verify_bundle).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-import(dig_ep_osm_exam_inward, [
	sort_candidate_docs/1,
	layout_candidate_edit/2,
	get_bundle_state/1
]).


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("Bundle Verification").

heading() ->
	title().

form() ->
	ep_osm_bundle.

%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, ?APPOSM_CONTROLLER) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, ?APPOSM_SCANUPLOADER) -> true;
access(_, ?APPOSM_QC) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=[
			itf:build(itf:hidden(?F(osm_exam_fk)), wf:q(examid)),
			itf:build(itf:hidden(?F(osm_bundle_fk)), wf:q(bundleid))
		],
		size=100
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	title().



%------------------------------------------------------------------------------
% function - init
%------------------------------------------------------------------------------
init() ->
	ok.


%------------------------------------------------------------------------------
% function - fetch
%------------------------------------------------------------------------------



%..............................................................................
%
% [osm_exam_fk, osm_bundle_fk]
%
%..............................................................................

fetch(D, _From, _Size, [
	#field {id=osm_exam_fk, uivalue=OsmExamId},
	#field {id=osm_bundle_fk, uivalue=OsmBundleId}
]) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(OsmExamId),
	{ok, ExamDoc} = ep_osm_exam_api:get(OsmExamId),
	{ok, BundleDoc} = ep_osm_bundle_api:get(OsmBundleId),



	%
	% get student docs from osm exam db with the specified bundle id
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY, [
		{use_index, ["osm_bundle_fk"]}
	]),
	CandidateDocs1 = sort_candidate_docs(CandidateDocs),



	%
	% results
	%
	Results = lists:map(fun(CDoc) ->
		[
			#dcell {val=layout_candidate(ExamDoc, BundleDoc, CDoc)}
		]
	end, CandidateDocs1),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Verify"}
	],


	%
	% return
	%
	{D#dig {
		total=length(CandidateDocs1),
		config=D#dig.config ++ [
			{show_slno, true}
		],
		description=#link {
			url=itx:format("/dig_ep_osm_exam_inward?id=~s", [OsmExamId]),
			text=io_lib:format("~s / ~s / ~s / Bundle: ~s (~s)", [
				itf:val(ExamDoc, anptestcourseid),
				itf:val(ExamDoc, testname),
				?LN(?L2A(itf:val(ExamDoc, teststatus))),
				itf:val(BundleDoc, number),
				get_bundle_state(BundleDoc)
			])
		}
	}, [Header] ++ Results};



%..............................................................................
%
% []
%
%..............................................................................
fetch(D, _From, _Size, _Fs) ->
	{D, []}.


%------------------------------------------------------------------------------
% function - exports
%------------------------------------------------------------------------------
exports() -> [
].



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	[
		dig:dig(?MODULE:get())
	].



%..............................................................................
%
% layout - candidate doc
%
%..............................................................................

layout_candidate(ExamDoc, _BundleDoc, CDoc) ->


	%
	% init
	%
	Fs = helper_api:doc2fields({ok, CDoc}),
	ImgUrls = anpcandidate:get_image_urls(
		itf:val(ExamDoc, aws_s3_dir), undefined, Fs
	),
	ImageUrl = case ImgUrls of
		[First |_] ->
			First;
		_ ->
			[]
	end,



	%
	% layout
	%
	#panel {
		class="border border-2 border-primary mb-5",
		body=[
			layout_candidate_sno(CDoc),
			layout_candidate_image(ImageUrl)
		]
	}.



%
% candidate - sno
%
layout_candidate_sno(CDoc) ->
	#h1 {
		class="display-3 fw-bold",
		text=itf:val(CDoc, anpseatnumber)
	}.


%
% candidate - image
%
layout_candidate_image(ImageUrl) ->
	#panel {
		style="height: 500px; overflow: scroll",
		body= #image {
			style="width: 100%",
			image=ImageUrl
		}
	}.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
