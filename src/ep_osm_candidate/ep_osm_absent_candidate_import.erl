-module(ep_osm_absent_candidate_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



%
% define
%
-define(INDEX_SUBJECT_CODE, 1).
-define(INDEX_SUBJECT_PATTERN, 3).
-define(INDEX_ANP_SEATNUMBER, 4).
-define(INDEX_ANP_PRN, 5).
-define(MAX_CSV_COL_SIZE, 5).


%..............................................................................
%
% layout - import absent data
%
%..............................................................................

layout_absent_student_data() ->
	Vals = [
		["subject_code", "Subject must exist"],
		["subject_name", "(Can be empty)"],
		["subject_pattern", "Pattern must exists"],
		["anp_paper_uid", "PRN/UID"],
		["anpseatnumber", "Seat number"]
	],
	Es = dig:layout_vals(#dig{}, Vals, ["Field", "Description"]),
	Fs = [
		?ITXF({sep, Es}),
		?COREXS(season_fk, #field {id=import_season_fk}),
		itf:attachment(?F(file_import_absent_student_data, "CSV file"))
	],
	itl:get(?EDIT, Fs, noevent, table).



%------------------------------------------------------------------------------
% handle import validate csv list
%------------------------------------------------------------------------------

handle_import_validate(List) ->
	ok = dig_mm_import_validator:handle_import_validate_csv_length(List, ?MAX_CSV_COL_SIZE),
	ok = ep_osm_candidate_import:handle_import_validate_csv_non_empty(List),
	ok = ep_osm_candidate_import:handle_import_validate_subjects_exist(List),
	ok = ep_osm_candidate_import:handle_import_validate_exams_exist(List),
	ok.



%------------------------------------------------------------------------------
% handle import validate batch
%------------------------------------------------------------------------------

handle_import_validate_batch(List) ->
	ok = handle_validate_batch_candidate_exists(List),
	ok = handle_import_validate_batch_anp_state_absent(List),
	ok.


%
% handle validate candidate exists
%
handle_validate_batch_candidate_exists(List) ->
	Dict = get_exam_wise_candidate_dict(List),
	[CandidateNotFound] = lists:map(fun({SubjectId, DocsToValidate}) ->
		SeasonId = minijobcontext:q(import_season_fk),
		[ExamDoc] = ep_osm_candidate_import:get_exam_docs(SeasonId, SubjectId),
		CandidateDocs = ep_osm_candidate_import:get_existing_candidate_docs(ExamDoc, DocsToValidate),
		CandidateDocsDict = ep_osm_candidate_import:get_existing_candidate_docs_dict(CandidateDocs, undefined),

		%
		% ensure candidates exists
		% before marking it as absent
		%
		validate_candidate_exists(DocsToValidate, CandidateDocsDict)

	end, dict:to_list(Dict)),

	?ASSERT(
		import_validation,
		CandidateNotFound == [],
		itx:format("Candidate ~p not found", [CandidateNotFound])
	).




%
% ensure candidate exists
%
validate_candidate_exists(DocsToValidate, CandidateDocsDict) ->
	lists:foldl(fun(Doc, AccErrors) ->
		case dict:find(itf:val(Doc, anpseatnumber), CandidateDocsDict) of
			{ok, _} -> AccErrors;
			_ -> AccErrors ++ [itf:val(Doc, anpseatnumber)]
		end
	end, [], DocsToValidate).



%
% validate validate batch absent anpstate
%
handle_import_validate_batch_anp_state_absent(List) ->
	Dict = get_exam_wise_candidate_dict(List),
	[{_, InvalidAnpState}] = lists:map(fun({SubjectId, DocsToValidate}) ->
		SeasonId = minijobcontext:q(import_season_fk),
		[ExamDoc] = ep_osm_candidate_import:get_exam_docs(SeasonId, SubjectId),
		CandidateDocs = ep_osm_candidate_import:get_existing_candidate_docs(ExamDoc, DocsToValidate),
		CandidateDocsDict = ep_osm_candidate_import:get_existing_candidate_docs_dict(CandidateDocs, undefined),

		%
		% validate candidate state
		% must be expected for marking it as absent
		%
		validate_candidate_state(DocsToValidate, CandidateDocsDict)

	end, dict:to_list(Dict)),

	?ASSERT(
		import_validation,
		InvalidAnpState == [],
		itx:format("The candidate ~p cannot be marked as absent as they are not in expected state.", [InvalidAnpState])
	).


%
% validate candidate state
%
validate_candidate_state(DocsToValidate, CandidateDocsDict) ->
	lists:foldl(fun(Doc, {AccOks, AccError}) ->
		validate_candidate_state(
			dict:find(itf:val(Doc, anpseatnumber), CandidateDocsDict),
			AccOks,
			AccError
		)
	end, {[], []}, DocsToValidate).

validate_candidate_state({ok, Doc}, AccOks, AccErrors) ->
	case itf:val(Doc, anpstate) of
		AnpState when
			AnpState == "anpstate_expected";
			AnpState == "anpstate_absent" ->
			{
				AccOks ++ [itf:val(Doc, anpseatnumber)],
				AccErrors
			};
		_ ->
			{
				AccOks,
				AccErrors ++ [itf:val(Doc, anpseatnumber)]
			}
	end.


%
% get test wise candidate dict
%
get_exam_wise_candidate_dict(List) ->
	%
	% get subject dict
	%
	SubjectDict = dig_mm_import_helper:get_subject_doct_dict_by_pattern(List, 1),

	%
	% get csv[{subDocId, [csv]}, ....] with subjectcode
	%
	FsRes = lists:map(fun(Csv) ->
		SubjectCode = lists:nth(?INDEX_SUBJECT_CODE, Csv),
		SubjectPattern = lists:nth(?INDEX_SUBJECT_PATTERN, Csv),
		{ok, SubjectDoc} = dict:find({SubjectCode, SubjectPattern}, SubjectDict),
		Fs = [
			fields:build(anp_paper_uid, lists:nth(?INDEX_ANP_PRN, Csv)),
			fields:build(anpseatnumber, lists:nth(?INDEX_ANP_SEATNUMBER, Csv))
		],
		{itf:idval(SubjectDoc), Fs}
	end, List),


	lists:foldl(fun({SubjectId, FsList}, Acc) ->
		dict:append(SubjectId, helper_api:fields2doc(FsList), Acc)
	end, dict:new(), FsRes).




% ----------------------------------------------------------------------------
% handle import csv to fs - imports student master data
%------------------------------------------------------------------------------

handle_import_csv_to_fs(List) ->
	lists:map(fun(Csv) ->
		%
		% init
		%
		SubjectCode = lists:nth(?INDEX_SUBJECT_CODE, Csv),
		SubjectPattern = lists:nth(?INDEX_SUBJECT_PATTERN, Csv),
		%
		% build fs to save
		%
		FsToSave = [
			fields:build(anp_paper_uid, lists:nth(?INDEX_ANP_PRN, Csv)),
			fields:build(anpseatnumber, lists:nth(?INDEX_ANP_SEATNUMBER, Csv)),
			fields:build(anpstate, "anpstate_absent")
		],

		%
		% return final fs to save
		%
		{SubjectCode, SubjectPattern, FsToSave}
	end, List).


%------------------------------------------------------------------------------
% db
%------------------------------------------------------------------------------

%
% save
%
savebulk(LoLofFields) ->
	ep_osm_candidate_import:savebulk(LoLofFields).



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------