-module(ep_osm_bundle_fields).
-compile(export_all).
-include("records.hrl").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(osm_exam_fk = I) ->
	F = itf:textbox_picker(?F(I, "OSM Exam")),
	F#field {
		module=anptest,
		options=#search {
			title=?LN("Select OSM Exam"),
			db=anptests:getdb(),
			displayfs=[
				fields:get(anptestcourseid),
				fields:get(testname)
			],
			filterfs=anptest:fs(search),
			size=10
		}
	};

f(number = I) ->
	itf:textbox_int(?F(I, "Bundle Number"));

f(receivedby = I) ->
	itf:createdby(?F(I, "Received By"));

f(receivedon = I) ->
	itf:createdon(?F(I, "Received On"));

f(createdby = I) ->
	itf:createdby(?F(I, "Created By"));

f(createdon = I) ->
	itf:createdon(?F(I, "Created On"));


f(osm_bundle_fk = I) ->
	itf:textbox(?F(I, "Bundle Id"));

f({osm_bundle_fk = I, OsmExamId}) ->
	F = itf:textbox_picker(?F(I, "Bundle")),
	F#field {options=options({osm_bundle_fk, OsmExamId})};


f(scannedby = I) ->
	F = itf:textbox_picker(?F(I, "Scanned By")),
	F#field {options=options(I)};

f(qualityby = I) ->
	F = itf:textbox_picker(?F(I, "Uploaded By")),
	F#field {options=options(I)};

f(qcby = I) ->
	F = itf:textbox_picker(?F(I, "QC By")),
	F#field {options=options(I)};

f(inwardstate = I) ->
	itf:dropdown(?F(I, "Inward State"), options(I));


f(scanningstate = I) ->
	itf:dropdown(?F(I, "Scanning State"), options(I));


f(uploadstate = I) ->
	itf:dropdown(?F(I, "Upload State"), options(I));

f(qcstate = I) ->
	itf:dropdown(?F(I, "QC State"), options(I));

f(bundle_size = I) ->
	itf:textbox_readonly(?F(I, "Bundle Size"));


f(inward_date = I) ->
	itf:date(?F(I, "Inward Date"));

f(scanned_date = I) ->
	itf:date(?F(I, "Scanned Date"));

f(uploaded_date = I) ->
	itf:date(?F(I, "Uploaded Date"));

f(qc_date = I) ->
	itf:date(?F(I, "QC Date"));

f(comments = I) ->
	itf:notes(?F(I, "Comments"));

f(packet_number = I) ->
	itf:textbox(?F(I, "Packet Number"),
		[required, alphanumeric, validator(unique_packet_number)]);

f(packet_count = I) ->
	itf:textbox_int(?F(I, "Packet Count"));

f(rack_location = I) ->
	itf:textbox(?F(I, "Rack Location"), [required]);

f(O) -> throw(O).

%------------------------------------------------------------------------------
% validators
%------------------------------------------------------------------------------

validator(unique_packet_number) ->
	{
		"Must be unique in the test",
		fun(DocFun, V) ->
			ep_osm_bundle_api:check_packet_number_exists(V, DocFun())
		end,
		fun() ->
			case {wf:page_module(), wf:q(mode), wf:q(id)} of
				{ep_osm_bundle, ?EDIT, Id} ->
					ep_osm_bundle_api:get(Id);
				_ ->
					undefined
			end
		end
	};

validator(O) ->
	throw(O).

%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------

options({osm_bundle_fk, OsmExamId}) ->
	#search {
		title=?LN("Select Bundle"),
		db=ep_osm_bundle:db(),
		displayfs_picked=[
			?OSMBDL(number),
			?OSMBDL(packet_number),
			?OSMBDL(rack_location)
		],
		displayfs=[
			?OSMBDL(number),
			?OSMBDL(packet_number),
			?OSMBDL(packet_count),
			?OSMBDL(rack_location),
			?OSMBDL(inwardstate),
			?OSMBDL(scanningstate),
			?OSMBDL(uploadstate),
			?OSMBDL(qcstate)
		],
		filterfs=[
			itf:build(itf:hidden(?F(osm_exam_fk)), OsmExamId),
			?OSMBDL(number),
			?OSMBDL(packet_number),
			?OSMBDL(rack_location)
		],
		size=10,
		searchfn=fun fetch_osm_bundle_fk/5
	};

options(qcby) ->
	#search {
		title=?LN("Select User"),
		db=itxprofiles:db(),
		displayfs=[
			?ITXPRF(username),
			?ITXPRF(email),
			?ITXPRF(mobile)
		],
		filterfs=[
			itf:build(?ITXPRF(profiletype), ?APPOSM_QC),
			?ITXPRF(username),
			?ITXPRF(email),
			?ITXPRF(mobile)
		],
		size=10
	};

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


options(State) when 
	State == inwardstate;
	State == scanningstate;
	State == uploadstate;
	State == qcstate ->
	itf:options([
		?F(new, "New / Unassigned"),
		?F(assigned, "Assigned"),
		?F(completed, "Completed"),
		?F(discarded, "Discarded")
	]).

%------------------------------------------------------------------------------
% renderers
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

fetch_osm_bundle_fk(Db, FsFind, From, Size, _SortFs) ->
	FsFind1 = dig:get_nonempty_fs(FsFind),
	{
		db:count(Db),
		ep_osm_bundle_api:fetch(?S2I(From), ?S2I(Size), FsFind1, [
			{use_index, ["osm_exam_fk"]}
		])
	}.

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

