-module(ep_osm_apt_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



%------------------------------------------------------------------------------
% db
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% handle import validate csv list
%------------------------------------------------------------------------------

handle_import_validate(List) ->
	ok = ep_osm_exam_import:handle_import_validate_seasonid(),
	ok = dig_mm_import_validator:handle_import_validate_csv_length(List, 9),
	ok = dig_mm_import_validator:handle_import_validate_csv_non_empty(List),
	ok = handle_import_validate_duplicates(List),
	ok = dig_mm_import_validator:handle_import_validate_mobile(List, 3),
	ok = dig_mm_import_validator:handle_import_validate_email(List, 4),
	ok = dig_mm_import_validator:handle_import_validate_faculties_exist(List, 6),
	ok = dig_mm_import_validator:handle_import_validate_programs_exist(List, 7),
	ok = dig_mm_import_validator:handle_import_validate_subjects_patterns_exist(List, 8, 9),
	ok = handle_import_validate_profiles_exist(List),
	ok.



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
		Key = {lists:nth(1, Csv), lists:nth(8, Csv), lists:last(Csv)},
		dict:update_counter(Key, 1, Acc)
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
% validate profiles exist
%
%..............................................................................

handle_import_validate_profiles_exist(List) ->

	%
	% get usernames dict
	%
	Usernames = lists:map(fun([Username | _]) ->
		Username
	end, List),
	UsernamesUnique = helper:unique(Usernames),
	ProfileDocs = profiles:getdocs_by_usernames(UsernamesUnique),
	ProfileDocsDict = helper:get_dict_from_docs(ProfileDocs, username),


	%
	% find not found
	%
	UsernamesNotFound = lists:foldl(fun(Username, Acc) ->
		case dict:find(Username, ProfileDocsDict) of
			{ok, _} ->
				Acc;
			error ->
				Acc ++ [Username]
		end
	end, [], UsernamesUnique),



	%
	% assert
	%
	?ASSERT(
		import_validation,
		UsernamesNotFound == [],
		{username_not_found, UsernamesNotFound}
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
	% create profiles dict by usernmae
	%
	Usernames = lists:map(fun([Username | _]) ->
		Username
	end, List),
	ProfileDocs = profiles:getdocs_by_usernames(Usernames),
	ProfileDocsDict = helper:get_dict_from_docs(ProfileDocs, username),


	%
	% get dicts
	%
	FacultyDocsDict = dig_mm_import_helper:get_facultydocs_dict(List, 6),
	ProgramDocsDict = dig_mm_import_helper:get_programdocs_dict(List, 7),
	SubjectDocsDict = dig_mm_import_helper:get_subject_docs_dict_by_pattern(List, 8),

	%
	% update fields
	%
	AptCount = db:count(ep_osm_apt_api:db()),
	{LoLFs, _ } = lists:foldl(fun([
		Username,
		_Fullname,
		_Mobile,
		_Email,
		_IP,
		FacultyCode,
		ProgramCode,
		SubjectCode,
		SubjectPattern
	], {Acc, AptCountAcc}) ->

		%
		% init
		%
		{ok, ProfileDoc} = dict:find(Username, ProfileDocsDict),
		{ok, FacultyDoc} = dict:find(FacultyCode, FacultyDocsDict),
		{ok, ProgramDoc} = dict:find(ProgramCode, ProgramDocsDict),

		SubjectToFind = case string:to_upper(SubjectPattern) of
			"NA" ->
				{SubjectCode, []};
			_ ->
				{SubjectCode, SubjectPattern}
		end,

		{ok, SubjectDoc} = dict:find(SubjectToFind, SubjectDocsDict),



		%
		% get fields for each subjectid
		%
		AptNumber = ?I2S(AptCountAcc),
		case handle_get_apt_fs(ProfileDoc, FacultyDoc, ProgramDoc, SubjectDoc, AptNumber) of
			[] ->
				{Acc, AptCountAcc};
			Fs ->
				{Acc ++ [Fs], AptCountAcc+1}
		end


	end, {[], AptCount}, List),
	LoLFs.


%..............................................................................
%
% handle get apt fs
%
%..............................................................................
handle_get_apt_fs(ProfileDoc, FacultyDoc, ProgramDoc, SubjectDoc, AptNumber) ->


	%
	% init
	%
	SeasonId = get_import_season_fk(),
	ProfileId = itf:idval(ProfileDoc),
	FacultyId = itf:idval(FacultyDoc),
	ProgramId = itf:idval(ProgramDoc),
	SubjectId = itf:idval(SubjectDoc),
	ProfileType = itf:val(ProfileDoc, profiletype),


	%
	% check if appointment already exists
	%
	FsFind = [
		itf:build(?COREXS(season_fk), get_import_season_fk()),
		itf:build(?CORSUB(subject_code_fk), SubjectId),
		itf:build(?OSMAPT(evaluator_id), ProfileId)
	],
	AptDocs = ep_osm_apt_api:fetch(0, 1, FsFind, [
		{use_index, ["evaluator_id"]}
	]),



	%
	% return fields or empty
	%
	case AptDocs of
		[] -> [
			itf:build(?COREXS(season_fk), SeasonId),
			itf:build(?CORFAC(faculty_code_fk), FacultyId),
			itf:build(?CORPGM(program_code_fk), ProgramId),
			itf:build(?CORSUB(subject_code_fk), SubjectId),
			itf:build(?OSMAPT(apt_number), AptNumber),
			itf:build(?OSMAPT(apt_state), "new"),
			itf:build(?OSMAPT(evaluator_id), ProfileId),
			itf:build(?OSMAPT(evaluator_type), ProfileType),
			itf:build(?OSMAPT(evaluator_state), "noresponse")
		];
		_ ->
			[]
	end.



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


%
% remove empty strings
%
remove_empty_str(List) ->
	lists:filter(fun(S) ->
		S /= []
	end, List).


%
% get import season fk
%

get_import_season_fk() ->
	minijobcontext:q(import_season_fk).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
