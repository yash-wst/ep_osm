-module(ep_osm_candidate_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



%------------------------------------------------------------------------------
% db
%------------------------------------------------------------------------------

savebulk(LoLofFields) ->

	%
	% groups by subject
	%
	Dict = lists:foldl(fun({SubjectCode, SubjectPattern, Fs}, Acc) ->
		dict:append(
			{SubjectCode, SubjectPattern},
			helper_api:fields2doc(Fs),
			Acc
		)
	end, dict:new(), LoLofFields),


	%
	% save docs
	%
	{Oks, Errors} = lists:foldl(fun(
		{{SubjectCode, SubjectPattern}, DocsToSave},
		{AccOks, AccErrors}
	) ->

		%
		% get subject doc
		%
		SubjectDocs = ep_core_subject_api:getdocs_by_subject_codes(
			[SubjectCode]
		),
		[SubjectDoc] = lists:filter(fun(SubjectDoc) ->
			itf:val(SubjectDoc, pattern) == SubjectPattern
		end, SubjectDocs),


		%
		% get exam doc
		%
		SeasonId = minijobcontext:q(import_season_fk),
		SubjectId = itf:idval(SubjectDoc),
		[ExamDoc] = get_exam_docs(SeasonId, SubjectId),


		%
		% get existing candidate docs
		%
		ExistingCandidateDocs = get_existing_candidate_docs(ExamDoc, DocsToSave),
		ExistingCandidateDocsDict = get_existing_candidate_docs_dict(ExistingCandidateDocs),


		%
		% filter out existing docs
		%
		DocsToSave1 = lists:filter(fun(Doc) ->

			PRN = itf:val(Doc, anp_paper_uid),
			SNO = itf:val(Doc, anpseatnumber),
		
			(
				(dict:find(PRN, ExistingCandidateDocsDict) == error) and
				(dict:find(SNO, ExistingCandidateDocsDict) == error)
			)


		end, DocsToSave),


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
% handlers
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% handle import validate csv list
%------------------------------------------------------------------------------

handle_import_validate(List) ->
	ok = handle_import_validate_csv_length(List),
	ok = handle_import_validate_csv_non_empty(List),
	ok = handle_import_validate_duplicates(List),
	ok = handle_import_validate_subjects_exist(List),
	ok = handle_import_validate_exams_exist(List),
	ok.



%..............................................................................
%
% validate csv length
%
%..............................................................................

handle_import_validate_csv_length(List) ->

	%
	% find out errors
	%
	{_Oks, Errors} = lists:foldl(fun(Csv, {AccOKs, AccErrors}) ->
		case length(Csv) == 7 of
			true ->
				{AccOKs ++ [Csv], AccErrors};
			_ ->
				{AccOKs, AccErrors ++ [Csv]}
		end
	end, {[], []}, List),


	%
	% assert validation
	%
	?ASSERT(
		import_validation,
		Errors == [],
		{invalid_csv_length, Errors}
	).



%..............................................................................
%
% validate csv non empty
%
%..............................................................................

handle_import_validate_csv_non_empty(List) ->

	%
	% find out errors
	%
	{_Oks, Errors} = lists:foldl(fun(Csv, {AccOKs, AccErrors}) ->
		
		%
		% subject name and pattern are optional
		%
		[SubjectCode, _SubjectName, _SubjectPattern | Tail] = Csv,
		Csv1 = [SubjectCode | Tail],


		%
		% check for empty fields
		%
		case lists:filter(fun(C) -> C == [] end, Csv1) of
			[] ->
				{AccOKs ++ [Csv], AccErrors};
			_ ->
				{AccOKs, AccErrors ++ [Csv]}
		end

	end, {[], []}, List),


	%
	% assert validation
	%
	?ASSERT(
		import_validation,
		Errors == [],
		{csv_non_empty, Errors}
	).



%..............................................................................
%
% validate duplicates
%
%..............................................................................

handle_import_validate_duplicates(List) ->

	%
	% find out errors
	%
	Dict = lists:foldl(fun(Csv, Acc) ->
		[
			SubjectCode,
			_SubjectName,
			_SubjectPattern,
			_CentreCode,
			PRN,
			SeatNumber | _
		] = Csv,
		
		Acc1 = dict:update_counter({SubjectCode, PRN}, 1, Acc),
		dict:update_counter({SubjectCode, SeatNumber}, 1, Acc1)
	
	end, dict:new(), List),



	%
	% find duplicates
	%
	Errors = lists:foldl(fun({Key, Count}, Acc) ->
		case Count > 1 of
			true ->
				Acc ++ [Key];
			_ ->
				Acc
		end
	end, [], dict:to_list(Dict)),


	%
	% assert validation
	%
	?ASSERT(
		import_validation,
		Errors == [],
		{duplicates_found, Errors}
	).




%..............................................................................
%
% validate subjects exist
%
%..............................................................................

handle_import_validate_subjects_exist(List) ->

	%
	% get subject codes
	%
	Subjects = lists:foldl(fun([SubjectCode, _SubjectName, SubjectPattern | _], Acc) ->
		Acc ++ [{SubjectCode, SubjectPattern}]
	end, [], List),
	{SubjectCodes, _SubjectPatterns} = lists:unzip(Subjects),
	SubjectCodesUnique0 = helper:unique(SubjectCodes),
	SubjectCodesUnique = remove_empty_str(SubjectCodesUnique0),



	%
	% get subject docs
	%
	IdFn = fun(Doc) ->
		{itf:val(Doc, subject_code), itf:val(Doc, pattern)}
	end,
	SubjectDocs = ep_core_subject_api:getdocs_by_subject_codes(SubjectCodesUnique),
	SubjectDocsDict = helper:get_dict_from_docs(SubjectDocs, IdFn),



	%
	% find not found
	%
	SubjectsUnique = helper:unique(Subjects),
	SubjectCodesNotFound = lists:foldl(fun(Subject, Acc) ->
		case dict:find(Subject, SubjectDocsDict) of
			{ok, _} ->
				Acc;
			error ->
				Acc ++ [Subject]
		end
	end, [], SubjectsUnique),



	%
	% assert
	%
	?ASSERT(
		import_validation,
		SubjectCodesNotFound == [],
		{subject_not_found, SubjectCodesNotFound}
	).




%..............................................................................
%
% validate exam exist
%
%..............................................................................

handle_import_validate_exams_exist(List) ->


	%
	% get subject codes
	%
	Subjects = lists:foldl(fun([SubjectCode, _SubjectName, SubjectPattern | _], Acc) ->
		Acc ++ [{SubjectCode, SubjectPattern}]
	end, [], List),
	{SubjectCodes, _SubjectPatterns} = lists:unzip(Subjects),
	SubjectCodesUnique0 = helper:unique(SubjectCodes),
	SubjectCodesUnique = remove_empty_str(SubjectCodesUnique0),



	%
	% get subject docs
	%
	IdFn = fun(Doc) ->
		{itf:val(Doc, subject_code), itf:val(Doc, pattern)}
	end,
	SubjectDocs = ep_core_subject_api:getdocs_by_subject_codes(SubjectCodesUnique),
	SubjectDocsDict = helper:get_dict_from_docs(SubjectDocs, IdFn),



	%
	% find exams for each subject
	%
	SeasonId = minijobcontext:q(import_season_fk),
	SubjectsUnique = helper:unique(Subjects),
	ExamsErrors = lists:foldl(fun(Subject, Acc) ->

		{ok, SubjectDoc} = dict:find(Subject, SubjectDocsDict),
		ExamDocs = get_exam_docs(SeasonId, itf:idval(SubjectDoc)),

		case ExamDocs of
			[_ExamDoc] ->
				Acc;
			[] ->
				Acc ++ [{Subject, "no exams"}];
			_ ->
				Acc ++ [{Subject, "multiple exams"}]
		end

	end, [], SubjectsUnique),



	%
	% assert
	%
	?ASSERT(
		import_validation,
		ExamsErrors == [],
		{exam_errors, ExamsErrors}
	).





%------------------------------------------------------------------------------
% handle import validate batch
%------------------------------------------------------------------------------

handle_import_validate_batch(_List) ->
	ok.


%------------------------------------------------------------------------------
% handle import csv to fs
%------------------------------------------------------------------------------

handle_import_csv_to_fs(List) ->


	%
	% update fields
	%
	lists:map(fun([
		SubjectCode,
		_SubjectName,
		SubjectPattern,
		CentreCode,
		PRN,
		SeatNumber,
		Fullname
	]) ->

		%
		% build fs to save
		%
		FsToSave = [
			fields:build(anpcentercode, CentreCode),
			fields:build(anp_paper_uid, PRN),
			fields:build(anpseatnumber, SeatNumber),
			fields:build(anpfullname, Fullname),
			fields:build(anpstate, "anpstate_not_uploaded")
		],


		%
		% return final fs to save
		%
		{SubjectCode, SubjectPattern, FsToSave}


	end, List).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


remove_empty_str(List) ->
	lists:filter(fun(S) ->
		S /= []
	end, List).



get_exam_docs(SeasonId, SubjectId) ->
	ep_osm_exam_api:fetch(0, ?INFINITY, [
			fields:build(season_fk, SeasonId),
			fields:build(subject_code_fk, SubjectId),
			fields:build(teststatus, ?SCHEDULED)
		], [
			{use_index, ["subject_code_fk"]}
		]
	).



get_existing_candidate_docs(ExamDoc, DocsToSave) ->


	%
	% init
	%
	ExamId = itf:idval(ExamDoc),
	Db = anpcandidates:db(ExamId),


	%
	% get candidates by PRN
	%
	PRNs = lists:map(fun(Doc) ->
		itf:val(Doc, anp_paper_uid)
	end, DocsToSave),
	DocsPRNs = db2_find:getdocs_by_ids(Db, anp_paper_uid, PRNs),


	%
	% get docs by seat number
	%
	SeatNumbers = lists:map(fun(Doc) ->
		itf:val(Doc, anpseatnumber)
	end, DocsToSave),
	DocsSeatNumbers = db2_find:getdocs_by_ids(Db, anpseatnumber, SeatNumbers),

	%
	% return unique
	%
	helper:unique(DocsPRNs ++ DocsSeatNumbers).


get_existing_candidate_docs_dict(CandidateDocs) ->

	lists:foldl(fun(Doc, Acc) ->
		PRN = itf:val(Doc, anp_paper_uid),
		SNO = itf:val(Doc, anpseatnumber),

		Acc1 = case PRN of
			[] ->
				Acc;
			_ ->
				dict:store(PRN, Doc, Acc)
		end,

		Acc2 = case SNO of
			[] ->
				Acc1;
			_ ->
				dict:store(SNO, Doc, Acc1)
		end,

		Acc2


	end, dict:new(), CandidateDocs).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
