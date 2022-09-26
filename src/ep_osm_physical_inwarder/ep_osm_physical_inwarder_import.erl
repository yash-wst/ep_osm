-module(ep_osm_physical_inwarder_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

-define(CSV_LENGTH, 4).
-define(PK_COLUMN_NUMBER, 1). % Primary key is username
-define(USERNAME_COLUMN_NUMBER, 1).
-define(MOBILE_COLUMN_NUMBER, 3).
-define(EMAIL_COLUMN_NUMBER, 4).
-define(RANDOM_PASSWORD_LENGTH, 10).

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
			List, ?CSV_LENGTH),

	%
	% check if there are duplicate centre codes in csv
	%
	ok = dig_mm_import_validator:handle_import_validate_unique_primary_keys(
			List, ?PK_COLUMN_NUMBER),

	%
	% validate mobile numbers in csv
	%
	ok = dig_mm_import_validator:handle_import_validate_mobile(
			List, ?MOBILE_COLUMN_NUMBER),

	%
	% validate emails in csv
	%
	ok = dig_mm_import_validator:handle_import_validate_email(
			List, ?EMAIL_COLUMN_NUMBER),

	%
	% check usernames are new
	%
	ok = check_usernames_are_new(List),

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
	lists:map(fun(ListItem) ->
		%
		% init
		%
		[Username, FullName, Mobile, Email] = ListItem,

		%
		% build FsToSave
		%
		[
			itf:build(?ITXPRF(profiletype), ?APPOSM_PHYSICAL_INWARDER),
			itf:build(?ITXPRF(username), Username),
			itf:build(?ITXPRF(fullname), FullName),
			itf:build(?ITXPRF(mobile), Mobile),
			itf:build(?ITXPRF(email), Email),
			itf:build(?ITXPRF(password_bcrypt),
				helper:random_password(?RANDOM_PASSWORD_LENGTH))
		]
	end, List).



%------------------------------------------------------------------------------
% send username and password via sms and email to new cap admins
%------------------------------------------------------------------------------

after_save({ok, _ }, ListofFs) ->
	lists:foreach(fun(Fs) ->
		dig_mm_ep_osm_physical_inwarder:after_create(Fs, ok)
	end, ListofFs);
after_save(_SaveRes, ListofFs) -> ok.

%------------------------------------------------------------------------------
% Check usernames should not pre-exist in DB
%------------------------------------------------------------------------------

check_usernames_are_new(List) ->

	%
	% init
	%
	ListofUniqueUsernames = lists:map(fun(Line) ->
		lists:nth(?USERNAME_COLUMN_NUMBER, Line)
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
	Docs = ep_osm_physical_inwarder_api:fetch(0, ?INFINITY, FsFind, [
		{use_index, ["username"]}]),

	%
	% get cap centre codes of duplicates found
	%
	DuplicateUsernamesFound = lists:map(fun(Doc) ->
		itf:val(Doc, username)
	end, Docs),

	%
	% assert if above was not empty
	%
	?ASSERT(
		import_validation,
		DuplicateUsernamesFound == [],
		{"These usernames already exist", DuplicateUsernamesFound}
	).

