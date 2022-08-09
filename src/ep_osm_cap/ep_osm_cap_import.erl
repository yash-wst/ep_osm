
-module(ep_osm_cap_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


-define(CAP_CENTRE_CSV_LENGTH, 2). % centre code and name only
-define(CAP_CENTRE_PK_COLUMN_NUMBER, 1). % Primary key is centre code


%------------------------------------------------------------------------------
% check if any cap centre code in csv already exists in DB
% %----------------------------------------------------------------------------
 handle_validate_cap_centre_codes_are_new(List) ->

	%
	% init
	%
	ListofUniquePrimaryKeys = lists:map(fun(Line) ->
		lists:nth(?CAP_CENTRE_PK_COLUMN_NUMBER, Line)
	end, List),

	%
	% Fs
	%
	FsFind = [
		db2es_find:get_field_cond("$in", code, ListofUniquePrimaryKeys)
	],

	%
	% get cap centre docs of those keys from db
	%
	CapCentreDocs = ep_osm_cap_api:fetch(0, ?INFINITY, FsFind),

	%
	% get cap centre codes of duplicates found
	%
	DuplicateCapCentreCodesFound = lists:map(fun(Doc) ->
		itf:val(Doc, code)
	end, CapCentreDocs),

	%
	% assert if above was not empty
	%
	?ASSERT(
		import_validation,
		DuplicateCapCentreCodesFound == [],
		{"These CAP centre codes already exist", DuplicateCapCentreCodesFound}
	).


%------------------------------------------------------------------------------
% handle import validate csv
%------------------------------------------------------------------------------

handle_import_validate(List) ->
	%
	% check csv is non empty
	%
	ok = dig_mm_import_validator:handle_import_validate_csv_non_empty(List),

	%
	% check if csv has right number of columns
	%
	ok = dig_mm_import_validator:handle_import_validate_csv_length(List, fun(Csv) ->
		length(Csv) > ?CAP_CENTRE_CSV_LENGTH
	end),

	%
	% check if there are duplicate centre codes in csv
	%
	ok = dig_mm_import_validator:handle_import_validate_unique_primary_keys(
		List, ?CAP_CENTRE_PK_COLUMN_NUMBER),


	%
	% check none of the input cap centre codes are already present in db
	%
	ok = handle_validate_cap_centre_codes_are_new(List),

	%
	% all good
	%
	ok.


%------------------------------------------------------------------------------
% handle import validate batch
%------------------------------------------------------------------------------

handle_import_validate_batch(_List) ->
	ok.


%------------------------------------------------------------------------------
% handle import csv to fs
%------------------------------------------------------------------------------

handle_import_csv_to_fs(List) ->

	lists:map( fun(ListItem) ->
		[CAPCentreCode, CAPCentreName | IPs] = ListItem,
		[
			itf:build(?OSMCAP(code), CAPCentreCode),
			itf:build(?OSMCAP(name), CAPCentreName),
			itf:build(?OSMCAP(ips), IPs)
		]
	end, List).

