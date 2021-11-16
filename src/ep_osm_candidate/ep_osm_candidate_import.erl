-module(ep_osm_candidate_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



%------------------------------------------------------------------------------
% db
%------------------------------------------------------------------------------

savebulk(LoLofFields) ->

	%
	% build docs
	%
	Docs = lists:map(fun(Fs) ->
		helper_api:fields2doc(Fs)
	end, LoLofFields),
	?D(Docs),


	%
	% save docs
	%



	%
	% return save res
	%
	{[], []}.

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
	ok = handle_import_validate_student_does_not_exist(List),
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
			_SubjectCode,
			_SubjectName,
			_SubjectPattern,
			_CentreCode,
			PRN,
			SeatNumber | _
		] = Csv,
		
		Acc1 = dict:update_counter(PRN, 1, Acc),
		dict:update_counter(SeatNumber, 1, Acc1)
	
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
	SubjectCodes = lists:foldl(fun([SubjectCode | _], Acc) ->
		Acc ++ [SubjectCode]
	end, [], List),
	SubjectCodesUnique0 = helper:unique(SubjectCodes),
	SubjectCodesUnique = remove_empty_str(SubjectCodesUnique0),


	%
	% get subject docs
	%
	SubjectDocs = ep_core_subject_api:getdocs_by_subject_codes(SubjectCodesUnique),
	SubjectDocsDict = helper:get_dict_from_docs(SubjectDocs, subject_code),


	%
	% find not found
	%
	SubjectCodesNotFound = lists:foldl(fun(SubjectCode, Acc) ->
		case dict:find(SubjectCode, SubjectDocsDict) of
			{ok, _} ->
				Acc;
			error ->
				Acc ++ [SubjectCode]
		end
	end, [], SubjectCodesUnique),



	%
	% assert
	%
	?ASSERT(
		import_validation,
		SubjectCodesNotFound == [],
		{subject_not_found, SubjectCodesNotFound}
	).


%
% validate student does not exist in db
%
handle_import_validate_student_does_not_exist(List) ->
	%
	% assert
	%
	?ASSERT(
		import_validation,
		false,
		not_implemented
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
		SubjectName,
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
			fields:build(anpfullname, Fullname)
		],


		%
		% return final fs to save
		%
		FsToSave


	end, List).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


remove_empty_str(List) ->
	lists:filter(fun(S) ->
		S /= []
	end, List).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
