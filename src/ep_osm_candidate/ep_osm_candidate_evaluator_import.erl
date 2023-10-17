-module(ep_osm_candidate_evaluator_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



-define(SUBJECT_CODE_INDEX, index_of(subject_code)).
-define(SUBJECT_PATTERN_INDEX, index_of(pattern)).
-define(ANP_CENTERCODE_INDEX, index_of(anpcentercode)).
-define(ANP_PAPERUID_INDEX, index_of(anp_paper_uid)).
-define(ANP_SEATNUMBER_INDEX, index_of(anpseatnumber)).
-define(ANP_FULLNAME_INDEX, index_of(anpfullname)).
-define(ANP_STATE_INDEX, index_of(anpstate)).
-define(APPOSM_EVALUATOR_INDEX, index_of(profileidfk_anpevaluator)).
-define(MAX_CSV_COL_SIZE, length(fs(import))).


%------------------------------------------------------------------------------
% fs
%------------------------------------------------------------------------------

fs(import) -> [
	?CORSUB(subject_code),
	?CORSUB(subject_name),
	?CORSUB(pattern),
	fields:get(anpcentercode),
	fields:get(anp_paper_uid),
	fields:get(anpseatnumber),
	fields:get(anpfullname),
	fields:get(profileidfk_anpevaluator)
];

fs(merge) -> [
	fields:get(anpcentercode),
	fields:get(anp_paper_uid),
	fields:get(anpfullname),
	fields:get(anpstate),
	fields:get(profileidfk_anpevaluator)
];

fs({Doc, merge}) ->
	lists:filter(fun(F) ->
		itf:val(Doc, F#field.id) == []
	end, fs(merge));


fs(all) ->
	ep_osm_candidate:fs(all).




%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% handle import validate csv list
%------------------------------------------------------------------------------

handle_import_validate(List) ->
	ok = dig_mm_import_validator:handle_import_validate_csv_length(List, ?MAX_CSV_COL_SIZE),
	ok = dig_mm_import_validator:handle_import_validate_csv_non_empty(List),
	ok = ep_osm_candidate_import:handle_import_validate_subjects_exist(List),
	ok = ep_osm_candidate_import:handle_import_validate_exams_exist(List),
	ok = validate_evaluator_exists(List),
	ok.






%
% validate evaluator exists
%

validate_evaluator_exists(List) ->
	EvaluatorDict = get_evaluars_dict(List),
	{_Oks, Errors} = lists:foldl(fun(Csv, {AccOks, AccErrors}) ->
		Username = lists:nth(?APPOSM_EVALUATOR_INDEX, Csv),
		case dict:find(Username, EvaluatorDict) of
			{ok, _Doc} ->
				{AccOks ++ [Username], AccErrors};
			_ ->
				{AccOks, AccErrors ++ [Username]}
		end

	end, {[], []}, List),

	%
	% assert
	%
	?ASSERT(
		import_validation,
		Errors == [],
		{evaluator_not_found, Errors}
	).




%------------------------------------------------------------------------------
% handle import validate batch
%------------------------------------------------------------------------------

handle_import_validate_batch(_List) ->
	ok.


%------------------------------------------------------------------------------
% handle import csv to fs - imports student master data
%------------------------------------------------------------------------------

handle_import_csv_to_fs(List) ->
	EvaluatorDict = get_evaluars_dict(List),
	SubjectDict = get_subject_dict(List),

	%
	% fs index in csv
	%
	IndexSubjectCode = ?SUBJECT_CODE_INDEX,
	IndexSubjectPattern = ?SUBJECT_PATTERN_INDEX,
	IndexEvaluatorUsername = ?APPOSM_EVALUATOR_INDEX,
	IndexAnpcentercode = ?ANP_CENTERCODE_INDEX,
	IndexAnpSeatnumber = ?ANP_SEATNUMBER_INDEX,
	IndexPRN = ?ANP_PAPERUID_INDEX,
	IndexFullName = ?ANP_FULLNAME_INDEX,



	lists:map(fun(Csv) ->

		%
		% init
		%
		SubjectCode = lists:nth(IndexSubjectCode, Csv),
		SubjectPattern = lists:nth(IndexSubjectPattern, Csv),
		Evaluator = lists:nth(IndexEvaluatorUsername, Csv),
		{ok, SubjectDoc} = dict:find({SubjectCode, SubjectPattern}, SubjectDict),
		{ok, EvaluatorDoc} = dict:find(Evaluator, EvaluatorDict),

		SubjectId = itf:idval(SubjectDoc),
		EvaluatorId = itf:idval(EvaluatorDoc),

		%
		% build fs to save
		%
		FsToSave = [
			itf:build(?CORSUB(subject_code_fk), SubjectId),
			fields:build(profileidfk_anpevaluator, EvaluatorId),
			fields:build(anpcentercode, lists:nth(IndexAnpcentercode, Csv)),
			fields:build(anp_paper_uid, lists:nth(IndexPRN, Csv)),
			fields:build(anpseatnumber, lists:nth(IndexAnpSeatnumber, Csv)),
			fields:build(anpfullname, lists:nth(IndexFullName, Csv)),
			fields:build(anpstate, "anpstate_active")
		],

		{itf:idval(SubjectDoc), itf:idval(EvaluatorDoc), FsToSave}

	end, List).





%
% evaluators dict
%
get_evaluars_dict(List) ->
	EvalUsernames = lists:map(fun(Csv) ->
		lists:nth(?APPOSM_EVALUATOR_INDEX, Csv)
	end, List),

	ProfilesDocs = profiles:getdocs_by_usernames(
		helper:unique(EvalUsernames)
	),

	EvaluatorDocs = lists:filter(fun(Doc) ->
		itf:val(Doc, profiletype) == ?APPOSM_EVALUATOR
	end, ProfilesDocs),

	helper:get_dict_from_docs(EvaluatorDocs, username).




%
% subject dict
%
get_subject_dict(List) ->
	SubCodes = lists:map(fun(Csv) ->
		lists:nth(1, Csv)
	end, List),
	Docs = ep_core_subject_api:getdocs_by_subject_codes(
		helper:unique(SubCodes)
	),
	Key = fun(Doc) -> {
		itf:val(Doc, subject_code), itf:val(Doc, pattern)
	} end,

	helper:get_dict_from_docs(Docs, Key).





%
% fs index
%
index_of(FId) ->
	dig_mm_import_helper:get_field_index(FId, fs(import)).






%------------------------------------------------------------------------------
% db
%------------------------------------------------------------------------------


%
% save
%
savebulk(LoLofFields) ->

	%
	% groups by subject
	%
	Dict = lists:foldl(fun({SubjectId, EvalutorId, Fs}, Acc) ->
		dict:append(
			{SubjectId, EvalutorId},
			helper_api:fields2doc(Fs),
			Acc
		)
	end, dict:new(), LoLofFields),


	%
	% save docs
	%
	{Oks, Errors} = lists:foldl(fun(
		{{SubjectId, _EvalutorId}, DocsToSave},
		{AccOks, AccErrors}
	) ->

		%
		% get exam doc
		%
		SeasonId = minijobcontext:q(import_season_fk),
		[ExamDoc] = ep_osm_candidate_import:get_exam_docs(SeasonId, SubjectId),

		%
		% merge with existing docs
		%
		DocsToSave1 = ep_osm_candidate_import:handle_merge_with_existing_docs(
			ExamDoc, DocsToSave, profileidfk_anpevaluator
		),

		%
		% save
		%
		Db = anpcandidates:db(itf:idval(ExamDoc)),
		BatchSize = 100,
		{ResOks, ResErrors} = db:savebulk(Db, DocsToSave1, BatchSize),
		{AccOks ++ ResOks, AccErrors ++ ResErrors}


	end, {[], []}, dict:to_list(Dict)),



	%
	% return save res
	%
	{Oks, Errors}.



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
