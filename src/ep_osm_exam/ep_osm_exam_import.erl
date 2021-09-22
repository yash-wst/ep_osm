-module(ep_osm_exam_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% handle import validate csv list
%------------------------------------------------------------------------------

handle_import_validate(List) ->
	ok = handle_import_validate_seasonid(),
	ok = handle_import_validate_csv_length(List),
	ok = handle_import_validate_csv_non_empty(List),
	ok.



%..............................................................................
%
% validate season id
%
%..............................................................................

handle_import_validate_seasonid() ->

	SeasonId = wf:q(import_season_fk),
	?ASSERT(
		import_validation,
		((SeasonId /= []) and (SeasonId /= undefined)),
		{empty_season, "Season cannot be empty"}
	).


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
		case length(Csv) of
			14 ->
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
		% find empty values
		%
		{_, EmptyValues} = lists:foldl(fun(C, {Index, Acc}) ->
			case {C, Index} of
				{[], Index} when Index == 5; Index == 8 ->
					{Index +1, Acc};
				{[], _} ->
					{Index +1, Acc ++ [C]};
				_ ->
					{Index +1, Acc}
			end
		end, {1, []}, Csv),


		%
		% error if empty values found
		%
		case EmptyValues of
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



%------------------------------------------------------------------------------
% handle import validate batch
%------------------------------------------------------------------------------

handle_import_validate_batch(List) ->
	handle_import_ensure_faculty(List),
	handle_import_ensure_program(List),
	handle_import_ensure_subject(List),
	ok.



%..............................................................................
%
% ensure faculty exists
%
%..............................................................................

handle_import_ensure_faculty(List) ->

	%
	% get codes
	%
	CodesDict = lists:foldl(fun(Csv, Acc) ->
		Key = lists:nth(1, Csv),
		Value = lists:nth(2, Csv),
		dict:store(Key, Value, Acc)
	end, dict:new(), List),
	CodesUnique = dict:fetch_keys(CodesDict),



	%
	% get docs
	%
	Docs = ep_core_faculty_api:getdocs_by_faculty_codes(CodesUnique),
	DocsDict = helper:get_dict_from_docs(Docs, faculty_code),



	%
	% check not found
	%
	CodesNotFound = lists:foldl(fun(Code, Acc) ->
		case dict:find(Code, DocsDict) of
			{ok, _} ->
				Acc;
			_ ->
				Acc ++ [Code]
		end
	end, [], CodesUnique),



	%
	% create missing
	%
	lists:foreach(fun(Code) ->


		%
		% init
		%
		{ok, Name} = dict:find(Code, CodesDict),
		FsToSave = [
			itf:build(?CORFAC(faculty_code), Code),
			itf:build(?CORFAC(faculty_name), Name)
		],


		%
		% save
		%
		case ep_core_faculty_api:save(FsToSave) of
			{ok, _} ->
				ok;
			_ ->
				?ASSERT(
					import_validation,
					false,
					{ensure_faculty_exists, "could not create: " ++ Code}
				)
		end

	end, CodesNotFound),


	%
	% faculty ok
	%
	ok.




%..............................................................................
%
% ensure program exists
%
%..............................................................................

handle_import_ensure_program(List) ->

	%
	% get codes
	%
	CodesDict = lists:foldl(fun(Csv, Acc) ->
		Key = lists:nth(3, Csv),
		Value = lists:nth(4, Csv),
		dict:store(Key, Value, Acc)
	end, dict:new(), List),
	CodesUnique = dict:fetch_keys(CodesDict),



	%
	% get docs
	%
	Docs = ep_core_program_api:getdocs_by_program_codes(CodesUnique),
	DocsDict = helper:get_dict_from_docs(Docs, program_code),



	%
	% check not found
	%
	CodesNotFound = lists:foldl(fun(Code, Acc) ->
		case dict:find(Code, DocsDict) of
			{ok, _} ->
				Acc;
			_ ->
				Acc ++ [Code]
		end
	end, [], CodesUnique),



	%
	% create missing
	%
	lists:foreach(fun(Code) ->


		%
		% init
		%
		{ok, Name} = dict:find(Code, CodesDict),
		FsToSave = [
			itf:build(?CORPGM(program_code), Code),
			itf:build(?CORPGM(program_name), Name)
		],


		%
		% save
		%
		case ep_core_program_api:save(FsToSave) of
			{ok, _} ->
				ok;
			_ ->
				?ASSERT(
					import_validation,
					false,
					{ensure_program_exists, "could not create: " ++ Code}
				)
		end

	end, CodesNotFound),


	%
	% program ok
	%
	ok.



%..............................................................................
%
% ensure subject exists
%
%..............................................................................

handle_import_ensure_subject(List) ->

	%
	% get codes
	%
	CodesDict = lists:foldl(fun(Csv, Acc) ->
		Key = lists:nth(6, Csv),
		Value = lists:nth(7, Csv),
		dict:store(Key, Value, Acc)
	end, dict:new(), List),
	CodesUnique = dict:fetch_keys(CodesDict),



	%
	% get docs
	%
	Docs = ep_core_subject_api:getdocs_by_subject_codes(CodesUnique),
	DocsDict = helper:get_dict_from_docs(Docs, subject_code),



	%
	% check not found
	%
	CodesNotFound = lists:foldl(fun(Code, Acc) ->
		case dict:find(Code, DocsDict) of
			{ok, _} ->
				Acc;
			_ ->
				Acc ++ [Code]
		end
	end, [], CodesUnique),



	%
	% create missing
	%
	lists:foreach(fun(Code) ->


		%
		% init
		%
		{ok, Name} = dict:find(Code, CodesDict),
		FsToSave = [
			itf:build(?CORSUB(subject_code), Code),
			itf:build(?CORSUB(subject_name), Name)
		],


		%
		% save
		%
		case ep_core_subject_api:save(FsToSave) of
			{ok, _} ->
				ok;
			_ ->
				?ASSERT(
					import_validation,
					false,
					{ensure_subject_exists, "could not create: " ++ Code}
				)
		end

	end, CodesNotFound),


	%
	% subject ok
	%
	ok.




%------------------------------------------------------------------------------
% handle import csv to fs
%------------------------------------------------------------------------------

handle_import_csv_to_fs(List) ->


	%
	% init
	%
	SeasonId = wf:q(import_season_fk),
	?D(SeasonId),
	{ok, SeasonDoc} = ep_core_exam_season_api:get(SeasonId),
	SeasonCode = itf:val(SeasonDoc, code),
	?ASSERT(
		SeasonCode /= [],
		"season code is empty!"
	),

	%
	% get dicts
	%
	DictFac = get_docs_dict(List, 1, faculty_code, fun ep_core_faculty_api:getdocs_by_faculty_codes/1),
	DictPgm = get_docs_dict(List, 3, program_code, fun ep_core_program_api:getdocs_by_program_codes/1),
	DictSub = get_docs_dict(List, 6, subject_code, fun ep_core_subject_api:getdocs_by_subject_codes/1),


	%
	% build fs list
	%
	lists:map(fun([
		FacultyCode,
		_FacultyName,
		ProgramCode,
		_ProgramName,
		_ProgramPattern,
		SubjectCode,
		_SubjectName,
		_SubjectPattern,
		TestName,
		ANPCourseId,
		TestTotalMarks,
		TestDuration,
		PagesPerBooklet,
		Startdate
	]) ->

		%
		% init
		%
		{ok, FacDoc} = dict:find(FacultyCode, DictFac),
		{ok, PgmDoc} = dict:find(ProgramCode, DictPgm),
		{ok, SubDoc} = dict:find(SubjectCode, DictSub),
		S3Dir = ep_osm_exam_api:s3dir(
			SeasonCode, itf:val(SubDoc, subject_code)
		),


		%
		% fs
		%
		[
			fields:build(season_fk, SeasonId),
			fields:build(faculty_code_fk, itf:idval(FacDoc)),
			fields:build(program_code_fk, itf:idval(PgmDoc)),
			fields:build(subject_code_fk, itf:idval(SubDoc)),
			fields:build(aws_s3_dir, S3Dir),
			fields:build(anptestcourseid, ANPCourseId),
			fields:build(testname, TestName),
			fields:build(testdescription, TestName),
			fields:build(teststatus, ?SCHEDULED),
			fields:build(testtotalmarks, TestTotalMarks),
			fields:build(testduration, TestDuration),
			fields:build(startdate, Startdate),
			fields:build(enddate, Startdate),
			fields:build(pages_per_booklet, PagesPerBooklet)
		]

	end, List).






%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


%..............................................................................
%
% get docs dict
%
%..............................................................................

get_docs_dict(List, Index, CodeType, FetchFn) ->

	%
	% get codes
	%
	Codes = lists:map(fun(Csv) ->
		lists:nth(Index, Csv)
	end, List),
	CodesUnique = helper:unique(Codes),


	%
	% get docs
	%
	Docs = FetchFn(CodesUnique),
	DocsDict = helper:get_dict_from_docs(Docs, CodeType),


	%
	% return
	%
	DocsDict.





%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
