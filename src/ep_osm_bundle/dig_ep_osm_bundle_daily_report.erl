
-module(dig_ep_osm_bundle_daily_report).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("Daily Scanning Report").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=[
			?COREXS(season_fk),
			?OSMBDL(osm_exam_fk),
			?OSMBDL(inward_date),
			?OSMBDL(scanned_date),
			?OSMBDL(uploaded_date)
		],
		events=[
			ite:button(export, "CSV", {itx, {dig, export}})
		],
		size=1
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
% []
%
%..............................................................................
fetch(D, _From, _Size, []) ->
	{D, [{custom, #p {text="Please select fitlers and search again"}}]};


%..............................................................................
%
% Fs
%
%..............................................................................
fetch(D, From, Size, Fs) ->

	%
	% get bundles from filters
	%
	BundleDocs = ep_osm_bundle_api:fetch(From, Size, Fs),
	

	%
	% layout bundles
	%
	Results = layout_bundles(BundleDocs),


	%
	% ensure empty bundles do not terminate export
	%
	LenResults = length(Results),
	LenBundleDocs = length(BundleDocs),
	Results1 = if
		LenResults < Size, LenBundleDocs == Size ->
			[[] ||  _ <- lists:seq(1, LenBundleDocs)];
		true ->
			Results
	end,




	{D#dig {
		total=?INFINITY,
		dcell_headers=[
			#dcell {type=header, val="Exam Code"},
			#dcell {type=header, val="Exam Name"},
			#dcell {type=header, val="PRN"},
			#dcell {type=header, val="Seat Number"},
			#dcell {type=header, val="Fullname"},
			#dcell {type=header, val="Evaluation State"},
			#dcell {type=header, val="Bundle Number"},
			#dcell {type=header, val="Received By"},
			#dcell {type=header, val="Inward State"},
			#dcell {type=header, val="Inwarded Date"},
			#dcell {type=header, val="Scanned By"},
			#dcell {type=header, val="Scan State"},
			#dcell {type=header, val="Scanned Date"},
			#dcell {type=header, val="Uploaded By"},
			#dcell {type=header, val="Upload State"},
			#dcell {type=header, val="Uploaded Date"}
		]
	}, Results1}.



%------------------------------------------------------------------------------
% function - exports
%------------------------------------------------------------------------------
exports() -> [
].



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	dig:dig(?MODULE:get()).



%..............................................................................
%
% layout - bundle docs
%
%..............................................................................
layout_bundles(BundleDocs) ->
	lists:foldl(fun(BundleDoc, Acc) ->
		Acc ++ layout_bundle(BundleDoc)
	end, [], BundleDocs).



layout_bundle(BundleDoc) ->


	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(itf:val(BundleDoc, osm_exam_fk)),


	%
	% get bundle docs and layout values
	%
	CandidateDocs = dig_ep_osm_exam_inward:get_bundle_docs(
		itf:val(BundleDoc, osm_exam_fk),
		itf:idval(BundleDoc)
	),


	%
	% layout candidate docs
	%
	layout_candidate_docs(ExamDoc, BundleDoc, CandidateDocs).




%..............................................................................
%
% layout - candidate docs
%
%..............................................................................

layout_candidate_docs(ExamDoc, BundleDoc, CandidateDocs) ->

	%
	% init
	%
	CellsExam = layout_exam_doc(ExamDoc),
	CellsBundle =  layout_bundle_doc(BundleDoc),


	%
	% candidate row
	%
	lists:map(fun(CandidateDoc) ->
		CellsExam ++
		layout_candidate_doc(ExamDoc, BundleDoc, CandidateDoc) ++
		CellsBundle
	end, CandidateDocs).



layout_candidate_doc(_ExamDoc, _BundleDoc, CandidateDoc) ->
	FIds = [
		anp_paper_uid,
		anpseatnumber,
		anpfullname,
		anpstate
	],
	lists:map(fun(FId) ->
		#dcell {val=itf:val(CandidateDoc, FId)}
	end, FIds).



%..............................................................................
%
% layout - exam doc
%
%..............................................................................

layout_exam_doc(ExamDoc) ->
	[
		#dcell {val=itf:val(ExamDoc, anptestcourseid)},
		#dcell {val=itf:val(ExamDoc, testname)}
	].



%..............................................................................
%
% layout - bundle doc
%
%..............................................................................

layout_bundle_doc(BundleDoc) ->
	FsBundle = [
		?OSMBDL(number),
		?OSMBDL(createdby),
		?OSMBDL(inwardstate),
		?OSMBDL(inward_date),
		?OSMBDL(scannedby),
		?OSMBDL(scanningstate),
		?OSMBDL(scanned_date),
		?OSMBDL(qualityby),
		?OSMBDL(uploadstate),
		?OSMBDL(uploaded_date)
	],

	lists:map(fun(#field {id=FId}) ->
		#dcell {val=itf:val(BundleDoc, FId)}
	end, FsBundle).





%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event({itx, {dig, export} = E}) ->

	%
	% assert
	%
	assert_export_time(),
	ite:event(E);

event({itx, E}) ->
	ite:event(E).




%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% assert export time
%------------------------------------------------------------------------------

assert_export_time() ->
	%
	% init
	%
	{Hour, _, _} = erlang:time(),
	?ASSERT(
		(
			(Hour >= itxconfigs_cache:get2(dig_ep_osm_bundle_daily_report_after_hours, 18)) or
			(Hour < itxconfigs_cache:get2(dig_ep_osm_bundle_daily_report_before_hours, 6))
		),
		"Report can be exported only between 6pm and 6am"
	).




%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
