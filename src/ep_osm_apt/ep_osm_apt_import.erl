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
	ok = profile_anpevaluator_import:handle_import_validate_csv_length(List),
	ok = profile_anpevaluator_import:handle_import_validate_csv_non_empty(List),
	ok = profile_anpevaluator_import:handle_import_validate_duplicates(List),
	ok = dig_mm_import_validator:handle_import_validate_mobile(List, 3),
	ok = dig_mm_import_validator:handle_import_validate_email(List, 4),
	ok = profile_anpevaluator_import:handle_import_validate_subjects_exist(List),
	ok = handle_import_validate_profiles_exist(List),
	ok.


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
	% create subjects dict by subject code
	%
	SubjectCodes = lists:foldl(fun(Csv, Acc) ->
		Acc ++ lists:sublist(Csv, 6, length(Csv))
	end, [], List),
	SubjectCodesUnique = helper:unique(SubjectCodes),
	SubjectDocs = ep_core_subject_api:getdocs_by_subject_codes(SubjectCodesUnique),
	SubjectDocsDict = helper:get_dict_from_docs(SubjectDocs, subject_code),



	%
	% update fields
	%
	lists:foldl(fun([
		Username,
		_Fullname,
		_Mobile,
		_Email,
		_IP | UserSubjectCodes0
	], Acc) ->



		%
		% init
		%
		UserSubjectCodes = remove_empty_str(UserSubjectCodes0),
		{ok, ProfileDoc} = dict:find(Username, ProfileDocsDict),


		%
		% get subjects
		%
		SubjectDocs = lists:map(fun(UserSubjectCode) ->
			{ok, SubjectDoc} = dict:find(UserSubjectCode, SubjectDocsDict),
			SubjectDoc
		end, UserSubjectCodes),



		%
		% get fields for each subjectid
		%
		lists:foldl(fun(SubjectDoc, Acc1) ->
			case handle_get_apt_fs(ProfileDoc, SubjectDoc) of
				[] ->
					Acc1;
				Fs ->
					Acc1 ++ [Fs]
			end
		end, Acc, SubjectDocs)


	end, [], List).


%..............................................................................
%
% handle get apt fs
%
%..............................................................................
handle_get_apt_fs(ProfileDoc, SubjectDoc) ->


	%
	% init
	%
	SeasonId = get_import_season_fk(),
	ProfileId = itf:idval(ProfileDoc),
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
			itf:build(?CORSUB(subject_code_fk), SubjectId),
			itf:build(?OSMAPT(apt_number), helper:uidintstr()),
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
