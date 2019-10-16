
-module(dig_ep_osm_exam_inward).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Inward").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(osm_exam_fk = I) ->
	F = itf:textbox(?F(I, "OSM Exam")),
	F#field {
		renderer=fun(_Mode, _Event, #field {label=L, uivalue=Id}) ->
				{L, [
					#hidden {id=I, text=Id},
					layout_osm_exam_name(Id)
				]}
		end
	}.



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	%
	% init
	%
	OsmExamId = wf:q(id),


	#dig {
		module=?MODULE,
		filters=[
			itf:build(f(osm_exam_fk), OsmExamId),
			?OSMBDL({osm_bundle_fk, OsmExamId})
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Inward").



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
% [osm_exam_fk]
%
%..............................................................................
fetch(D, _From, _Size, [
	#field {id=osm_exam_fk, uivalue=OsmExamId}
] = Fs) ->

	%
	% init
	%
	#db2_find_response {docs=BundleDocs} = db2_find:get_by_fs(
		ep_osm_bundle_api:db(), Fs, 0, ?INFINITY
	),
	FBundle = ?OSMBDL({osm_bundle_fk, OsmExamId}),


	%
	% sort bundles by number
	%
	BundleDocsSorted = lists:sort(fun(A, B) ->
		itf:val(A, number) < itf:val(B, number)
	end, BundleDocs),



	%
	% results
	%
	Results = lists:map(fun(BDoc) ->
		[
			#dcell {val=itf:val(BDoc, number)},
			#dcell {val=itf:val(BDoc, createdby)},
			#dcell {val=itl:render(itf:d2f(BDoc, ?OSMBDL(createdon)))},
			#dcell {
				val=#span {
					class="btn btn-sm btn-primary-outline",
					body="Bundle #" ++ itf:val(BDoc, number)
				},
				postback={filter, itf:build(FBundle, itf:idval(BDoc))}
			}
		]
	end, BundleDocsSorted),


	%
	% actions
	%
	Actions = [
		{create_bundle, "Create New Bundle", "Create New Bundle"}
	],


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Bundle Number"},
		#dcell {type=header, val="Created By"},
		#dcell {type=header, val="Created On"},
		#dcell {type=header, val="Select"}
	],


	%
	% return
	%
	{D#dig {
		actions=Actions
	}, [Header] ++ Results};




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



	%
	% get student docs from osm exam db with the specified bundle id
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY
	),



	%
	% results
	%
	Results = lists:map(fun(CDoc) ->
		[
			#dcell {val=itf:val(CDoc, anp_paper_uid)},
			#dcell {val=itf:val(CDoc, anpseatnumber)},
			#dcell {val=itf:val(CDoc, anpfullname)}
		]
	end, CandidateDocs),


	%
	% actions
	%
	Actions = [
		{print_bundle_cover, "Print Bundle Cover", "Print Bundle Cover"},
		{refresh, "Refresh", "Refresh"},
		{form, layout_inward_form(), "Inward Form: (enter barcode or seat number and hit enter)"}
	],


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Barcode / UID"},
		#dcell {type=header, val="Seat No."},
		#dcell {type=header, val="Student Name"}
	],


	%
	% return
	%
	{D#dig {
		actions=Actions
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
% layout - inward form
%
%..............................................................................

layout_inward_form() ->
	Fs = [
		itf:textbox(?F(anp_paper_uid, "Barcode / UID"), [], textbox_enterkey),
		itf:textbox(?F(anpseatnumber, "Student Seat No."), [], textbox_enterkey)
	],
	itl:get(?CREATE, Fs, noevent, line).


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(textbox_enterkey) ->
	handle_inward(wf:q(anp_paper_uid), wf:q(anpseatnumber));


event(refresh) ->
	dig:refresh();


event({confirmation_yes, create_bundle}) ->
	handle_create_bundle(wf:q(osm_exam_fk));


event(create_bundle) ->
	itl:confirmation(
		#panel {class="mycenter", body=[
			#p {text="Are you sure you want to create a new bundle?"},
			#p {text="Please create new bundle only after previous bundle is full"}
		]},
		create_bundle
	);


event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------

%..............................................................................
%
% handle - inward
%
%..............................................................................

handle_inward([], []) ->
	helper_ui:flash(error, "Please enter either barcode or student seat number", 5);

handle_inward(UId, SNo) ->

	%
	% init
	%
	ExamId = wf:q(id),
	OsmBundleId = wf:q(osm_bundle_fk),
	BundleNumber = OsmBundleId,
	ExamDb = anpcandidates:db(ExamId),



	%
	% assert - bundle is not full
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId)
	],
	#db2_find_response {docs=BundleDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY
	),
	?ASSERT(
		length(BundleDocs) < 101,
		"bundle full; create new bundle"
	),




	%
	% assert -  a document in the osm exam db with same uid or seat number does
	% not exist already
	%
	FsToSearchCandidate = [
		itf:build(itf:textbox(?F(anp_paper_uid)), UId),
		itf:build(itf:textbox(?F(anpseatnumber)), SNo)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchCandidate, 0, ?INFINITY
	),
	?ASSERT(
		CandidateDocs == [],
		io_lib:format("~s, ~s: already exists in the exam database", [
			UId, SNo
		])
	),


	%
	% create entry in exam db
	%

	FsToSave = [
		itf:build(itf:textbox(?F(anp_paper_uid)), UId),
		itf:build(itf:textbox(?F(anpseatnumber)), ?CASE_IF_THEN_ELSE(SNo, [], UId, SNo)),
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId),
		itf:build(itf:textbox(?F(anpcentercode)), BundleNumber),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded")
	],
	case anpcandidates:save(ExamDb, FsToSave) of
		{ok, _} ->
			helper_ui:flash(success, io_lib:format("saved: ~s, ~s", [UId, SNo]));
		_ ->
			helper_ui:flash(error, io_lib:format("error: ~s, ~s", [UId, SNo]))
	end.



%..............................................................................
%
% handle - create bundle
%
%..............................................................................

handle_create_bundle(ExamId) ->

	%
	% init
	%
	FsToSave = [
		itf:build(?OSMBDL(osm_exam_fk), ExamId),
		itf:build(?OSMBDL(createdby), itxauth:user()),
		itf:build(?OSMBDL(createdon), helper:epochtimestr())
	],


	%
	% save
	%
	case ep_osm_bundle_api:create(FsToSave) of
		{ok, _BundleDoc} ->
			dig:refresh();
		_ ->
			helper_ui:flash(error, "Sorry, could not create bundle!")
	end.


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


layout_osm_exam_name(undefined) ->
	[];
layout_osm_exam_name(OsmExamId) ->
	%
	% init
	%
	{ok, ExamDoc} = anptests:getdoc(OsmExamId),
	itl:blockquote([
		ep_core_exam_season_api:getname(itf:val(ExamDoc, season_fk)),
		itf:val(ExamDoc, anptestcourseid),
		itf:val(ExamDoc, testname),
		?LN(?L2A(itf:val(ExamDoc, teststatus))),
		itf:val(ExamDoc, exam_pattern)
	]).



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------