
-module(ep_osm_cap_admin_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


-define(CAP_ADMIN_CSV_LENGTH, 5).
-define(CAP_ADMIN_PK_COLUMN_NUMBER, 1). % Primary key is username
-define(CAP_ADMIN_USERNAME_COLUMN_NUMBER, 1).
-define(CAP_ADMIN_MOBILE_COLUMN_NUMBER, 3).
-define(CAP_ADMIN_EMAIL_COLUMN_NUMBER, 4).
-define(CAP_ADMIN_CAP_CENTRE_COLUMN_NUMBER, 5).
-define(RANDOM_PASSWORD_LENGTH, 10).


%------------------------------------------------------------------------------
% retrieves list of cap centre docs of input cap centre codes from db
%------------------------------------------------------------------------------
get_cap_centre_docs_and_unique_codes_list(List) ->
	%
	% init
	%
	ListofCapCentreCodes = lists:map(fun(Line) ->
		lists:nth(?CAP_ADMIN_CAP_CENTRE_COLUMN_NUMBER, Line)
	end, List),

	%
	% get unique values
	%
	UniqueCapCentreCodes = helper:unique(ListofCapCentreCodes),

	%
	% Fs
	%
	FsFind = [
		db2es_find:get_field_cond("$in", code, UniqueCapCentreCodes)
	],

	%
	% get cap centre docs of those keys from db
	%
	CapCentreDocs = ep_osm_cap_api:fetch(0, ?INFINITY, FsFind),

	{UniqueCapCentreCodes, CapCentreDocs}.


%------------------------------------------------------------------------------
% check if any cap centre code in csv already exists in DB
% %----------------------------------------------------------------------------
 check_all_cap_centres_in_csv_exist_in_db(List) ->

 	%
 	% init
 	%
 	{UniqueCapCentreCodes, CapCentreDocs} =
 		get_cap_centre_docs_and_unique_codes_list(List),

	%
	% get cap centre codes found
	%
	CapCentreCodesFound = lists:map(fun(Doc) ->
		itf:val(Doc, code)
	end, CapCentreDocs),

	CapCentreCodesMissingInDB = UniqueCapCentreCodes -- CapCentreCodesFound,

	%
	% assert for missing cap centre codes in db
	%
	?ASSERT(
		import_validation,
		length(CapCentreCodesMissingInDB) == 0,
		{"These CAP centre codes are not registered", CapCentreCodesMissingInDB}
	).

%------------------------------------------------------------------------------
% Check usernames should not pre-exist in DB
%------------------------------------------------------------------------------

check_usernames_are_new(List) ->

	%
	% init
	%
	ListofUniqueUsernames = lists:map(fun(Line) ->
		lists:nth(?CAP_ADMIN_USERNAME_COLUMN_NUMBER, Line)
	end, List),

	%
	% Fs
	%
	FsFind = [
		db2es_find:get_field_cond("$in", username, ListofUniqueUsernames)
	],

	%
	% get cap admin docs
	%
	CapAdminDocs = ep_osm_cap_admin_api:fetch(0, ?INFINITY, FsFind, [
		{use_index, ["username"]}]),

	%
	% get cap centre codes of duplicates found
	%
	DuplicateUsernamesFound = lists:map(fun(Doc) ->
		itf:val(Doc, username)
	end, CapAdminDocs),

	%
	% assert if above was not empty
	%
	?ASSERT(
		import_validation,
		DuplicateUsernamesFound == [],
		{"These usernames already exist", DuplicateUsernamesFound}
	).



%------------------------------------------------------------------------------
% handle import validate csv
%------------------------------------------------------------------------------

handle_import_validate(List) ->
	%
	% check csv is non empty
	%
	ok = dig_mm_import_validator:handle_import_validate_csv_non_empty(
			List),

	%
	% check if csv has right number of columns
	%
	ok = dig_mm_import_validator:handle_import_validate_csv_length(
			List, ?CAP_ADMIN_CSV_LENGTH),

	%
	% check if there are duplicate centre codes in csv
	%
	ok = dig_mm_import_validator:handle_import_validate_unique_primary_keys(
			List, ?CAP_ADMIN_PK_COLUMN_NUMBER),

	%
	% validate mobile numbers in csv
	%
	ok = dig_mm_import_validator:handle_import_validate_mobile(
			List, ?CAP_ADMIN_MOBILE_COLUMN_NUMBER),

	%
	% validate emails in csv
	%
	ok = dig_mm_import_validator:handle_import_validate_email(
			List, ?CAP_ADMIN_EMAIL_COLUMN_NUMBER),

	%
	% check usernames are new
	%
	ok = check_usernames_are_new(List),

	%
	% check all of the input cap centre codes are already present in db
	%
	ok = check_all_cap_centres_in_csv_exist_in_db(List),

	%
	% all good
	%
	ok.


%------------------------------------------------------------------------------
% handle import validate batch
%------------------------------------------------------------------------------

handle_import_validate_batch(List) ->
	ok.


%------------------------------------------------------------------------------
% handle import csv to fs
%------------------------------------------------------------------------------

handle_import_csv_to_fs(List) ->
 	%
 	% init
 	%
 	{ _, CapCentreDocs } = get_cap_centre_docs_and_unique_codes_list(List),
	CapCentreDocsDict = helper:get_dict_from_docs(CapCentreDocs, code),


	lists:map(fun(ListItem) ->
		%
		% init
		%
		[Username, FullName, Mobile, Email, CAPCentreCode] = ListItem,

		%
		% get cap centre doc id
		%
		{ok, CapCentreDoc} = dict:find(CAPCentreCode, CapCentreDocsDict),
		CapCentreDocID = itf:idval(CapCentreDoc),

		%
		% build FsToSave
		%
		[
			itf:build(?ITXPRF(profiletype), ?APPOSM_CAPADMIN),
			itf:build(?ITXPRF(username), Username),
			itf:build(?ITXPRF(fullname), FullName),
			itf:build(?ITXPRF(mobile), Mobile),
			itf:build(?ITXPRF(email), Email),
			itf:build(?OSMCAP(osm_cap_fk), CapCentreDocID),
			itf:build(?ITXPRF(password_bcrypt),
				helper:random_password(?RANDOM_PASSWORD_LENGTH))
		]
	end, List).



%------------------------------------------------------------------------------
% send username and password via sms and email to new cap admins
%------------------------------------------------------------------------------

after_save({ok, _ }, ListofFs) ->
	lists:foreach(fun(Fs) ->
		dig_mm_ep_osm_cap_admin:after_create(Fs, ok)
	end, ListofFs);
after_save(_SaveRes, ListofFs) -> ok.
