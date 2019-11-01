-module(ep_osm_bundle_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



%------------------------------------------------------------------------------
% db
%------------------------------------------------------------------------------

savebulk(LoLofFields) ->
	ExamDb = anpcandidates:db(wf:q(osm_exam_fk)),
	anpcandidates:savebulk(ExamDb, LoLofFields).


%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% handle import validate csv list
%------------------------------------------------------------------------------

handle_import_validate(List) ->
	ok = handle_import_validate_csv_length(List),
	ok = handle_import_validate_csv_non_empty(List),
	ok = handle_import_validate_bundles_not_completed(List),
	ok = handle_import_validate_duplicates_seatnumbers(List),
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
		case length(Csv) of
			3 ->
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
		case lists:filter(fun(C) -> C == [] end, Csv) of
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
% validate bundles not completed
%
%..............................................................................

handle_import_validate_bundles_not_completed(List) ->


	%
	% init
	%
	ExamId = wf:q(osm_exam_fk),


	%
	% get seat numbers
	%
	BundleNumbers = lists:map(fun([_SubjectCode, BundleNumber, _SeatNumber]) ->
		BundleNumber
	end, List),
	BundleNumbersUnique = helper:unique(BundleNumbers),



	%
	% get bundle docs
	%
	BundleDocs = dig_ep_osm_exam_bundle:get_bundles(ExamId),
	BundleDocsDict = helper:get_dict_from_docs(BundleDocs, number),


	%
	% check completed bundles
	%
	BundlesCompleted = lists:foldl(fun(BundleNumber, Acc) ->

		%
		% get bundle doc
		%
		BundleDoc = case dict:find(BundleNumber, BundleDocsDict) of
			{ok, BundleDoc0} ->
				BundleDoc0;
			_ ->
				{[]}
		end,


		%
		% get bundle state
		%
		case itf:val(BundleDoc, inwardstate) of
			"completed" ->
				Acc ++ [itf:val(BundleDoc, number)];
			_ ->
				Acc
		end

	end, [], BundleNumbersUnique),


	%
	% assert validation
	%
	?ASSERT(
		import_validation,
		BundlesCompleted == [],
		{bundles_completed, BundlesCompleted}
	).



%..............................................................................
%
% validate duplicates seatnumbers
%
%..............................................................................

handle_import_validate_duplicates_seatnumbers(List) ->


	%
	% find out errors
	%
	DictSeatNumber = lists:foldl(fun([
		_SubjectCode, _BundleNumber, SeatNumber
	], Acc) ->
		case SeatNumber of
			[] ->
				Acc;
			_ ->
				dict:update_counter(SeatNumber, 1, Acc)
		end
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
	end, [], dict:to_list(DictSeatNumber)),


	%
	% assert validation
	%
	?ASSERT(
		import_validation,
		Errors == [],
		{duplicates_found, Errors}
	).





%------------------------------------------------------------------------------
% handle import validate batch
%------------------------------------------------------------------------------

handle_import_validate_batch(List) ->
	ok = handle_import_validate_batch_anpseatnumber(List),
	ok = handle_import_ensure_bundle_exists(List),
	ok.



%..............................................................................
%
% validate seat number
%
%..............................................................................

handle_import_validate_batch_anpseatnumber(List) ->


	%
	% init
	%
	ExamId = wf:q(osm_exam_fk),


	%
	% get seat numbers
	%
	SeatNumbers = lists:map(fun([_SubjectCode, _BundleNumber, SeatNumber]) ->
		SeatNumber
	end, List),
	SeatNumbersUnique = helper:unique(SeatNumbers),



	%
	% get candidate docs
	%
	CandidateDocs = anpcandidates:getdocs_by_snos(ExamId, SeatNumbersUnique),
	SeatNumbersFound = lists:map(fun(CandidateDoc) ->
		itf:val(CandidateDoc, anpseatnumber)
	end, CandidateDocs),

	%
	% assert
	%
	?ASSERT(
		import_validation,
		SeatNumbersFound == [],
		{exists_seat_numbers, SeatNumbersFound}
	).



%..............................................................................
%
% ensure bundle exists
%
%..............................................................................

handle_import_ensure_bundle_exists(List) ->


	%
	% init
	%
	ExamId = wf:q(osm_exam_fk),


	%
	% get seat numbers
	%
	BundleNumbers = lists:map(fun([_SubjectCode, BundleNumber, _SeatNumber]) ->
		BundleNumber
	end, List),
	BundleNumbersUnique = helper:unique(BundleNumbers),



	%
	% get bundle docs
	%
	BundleDocs = dig_ep_osm_exam_bundle:get_bundles(ExamId),
	BundleDocsDict = helper:get_dict_from_docs(BundleDocs, number),



	%
	% check not found bundles
	%
	BundleNumbersNotFound = lists:foldl(fun(BundleNumber, Acc) ->
		case dict:find(BundleNumber, BundleDocsDict) of
			{ok, _} ->
				Acc;
			_ ->
				Acc ++ [BundleNumber]
		end
	end, [], BundleNumbersUnique),



	%
	% create missing bundles
	%
	lists:foreach(fun(BundleNumber) ->
		%
		% init
		%
		FsToSave = [
			itf:build(?OSMBDL(osm_exam_fk), ExamId),
			itf:build(?OSMBDL(number), BundleNumber),
			itf:build(?OSMBDL(createdby), itxauth:user()),
			itf:build(?OSMBDL(createdon), helper:epochtimestr())
		],


		%
		% save
		%
		case ep_osm_bundle_api:create(FsToSave) of
			{ok, _} ->
				ok;
			_ ->
				?ASSERT(
					import_validation,
					false,
					{ensure_bundle_exists, "could not create bundle " ++ BundleNumber}
				)
		end

	end, BundleNumbersNotFound),



	%
	% all ok
	%
	ok.





%------------------------------------------------------------------------------
% handle import csv to fs
%------------------------------------------------------------------------------

handle_import_csv_to_fs(List) ->


	%
	% init
	%
	ExamId = wf:q(osm_exam_fk),


	%
	% get bundle docs
	%
	BundleDocs = dig_ep_osm_exam_bundle:get_bundles(ExamId),
	BundleDocsDict = helper:get_dict_from_docs(BundleDocs, number),



	%
	% build fs
	%
	FsList = lists:map(fun([
		_SubjectCode,
		BundleNumber,
		SeatNumber
	]) ->

		%
		% get bundle doc
		%
		{ok, BundleDoc} = dict:find(BundleNumber, BundleDocsDict),
		BundleId = itf:idval(BundleDoc),



		%
		% fs
		%
		[
			itf:build(itf:textbox(?F(anp_paper_uid)), []),
			itf:build(itf:textbox(?F(anpseatnumber)), SeatNumber),
			itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId),
			itf:build(itf:textbox(?F(anpcentercode)), BundleId),
			itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded")
		]

	end, List),


	%
	% return fs list
	%
	FsList.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
