-module(ep_osm_bundle_fields).
-compile(export_all).
-include("records.hrl").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(osm_exam_fk = I) ->
	F = itf:textbox_picker(?F(I, "OSM Exam")),
	F#field {
		options=#search {
			title=?LN("Select OSM Exam"),
			db=anptests:getdb(),
			displayfs=[
				fields:get(testname)
			],
			filterfs=anptest:fs(search),
			size=10
		}
	};

f(number = I) ->
	itf:textbox_int(?F(I, "Bundle Number"));

f(createdby = I) ->
	itf:createdby(?F(I, "Created By"));

f(createdon = I) ->
	itf:createdon(?F(I, "Created On"));


f(osm_bundle_fk = I) ->
	itf:textbox(?F(I, "Bundle Id"));

f({osm_bundle_fk = I, OsmExamId}) ->


	%
	% init
	%
	Fs = [
		itf:build(?OSMBDL(osm_exam_fk), OsmExamId)
	],


	%
	% get bundles for the specified exam season
	%
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		ep_osm_bundle_api:db(), Fs, 0, ?INFINITY
	),


	%
	% build options
	%
	Options = itf:options(lists:map(fun(D) ->
		{
			itf:idval(D),
			io_lib:format("~s - ~s", [itf:val(D, number), itf:val(D, createdby)])
		}
	end, Docs)),


	%
	% return dropdown
	%
	itf:dropdown(?F(I, "Bundle"), Options);



f(scannedby = I) ->
	F = itf:textbox_picker(?F(I, "Scanned By")),
	F#field {options=options(I)};

f(qualityby = I) ->
	F = itf:textbox_picker(?F(I, "QC/Uploaded By")),
	F#field {options=options(I)};


f(inwardstate = I) ->
	itf:dropdown(?F(I, "Inward State"), options(I));


f(scanningstate = I) ->
	itf:dropdown(?F(I, "Scanning State"), options(I));


f(uploadstate = I) ->
	itf:dropdown(?F(I, "Upload State"), options(I));


f(bundle_size = I) ->
	itf:textbox_readonly(?F(I, "Bundle Size"));


f(inward_date = I) ->
	itf:date(?F(I, "Inward Date"));

f(scanned_date = I) ->
	itf:date(?F(I, "Scanned Date"));

f(uploaded_date = I) ->
	itf:date(?F(I, "Uploaded Date"));



f(O) -> throw(O).

%------------------------------------------------------------------------------
% validators
%------------------------------------------------------------------------------
validator(O) ->
	throw(O).

%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------

options(Type) when Type == scannedby; Type == qualityby ->
	#search {
		title=?LN("Select User"),
		db=itxprofiles:db(),
		displayfs=[
			?ITXPRF(username),
			?ITXPRF(email),
			?ITXPRF(mobile)
		],
		filterfs=[
			itf:build(?ITXPRF(profiletype), ?APPOSM_SCANUPLOADER),
			?ITXPRF(username),
			?ITXPRF(email),
			?ITXPRF(mobile)
		],
		size=10
	};


options(State) when State == inwardstate; State == scanningstate; State == uploadstate ->
	itf:options([
		?F(new, "New / Unassigned"),
		?F(assigned, "Assigned"),
		?F(completed, "Completed")
	]).

%------------------------------------------------------------------------------
% renderers
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

