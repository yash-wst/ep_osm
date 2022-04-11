-module(ep_osm_scanuploader_fields).
-compile(export_all).
-include("records.hrl").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------


f(ep_osm_scanuploader_fk = I) ->
	F = itf:textbox_picker(?F(I, ?LN("DTP Staff"))),
	F#field {
		module=ep_osm_scanuploader,
		options=options(I)
	};


f(O) -> throw(O).

%------------------------------------------------------------------------------
% validators
%------------------------------------------------------------------------------
validator(O) ->
	throw(O).

%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------

options(ep_osm_scanuploader_fk) ->
	#search {
		datasource=couchdb,
		title=?LN("Select Scanner / Uploader"),
		db=ep_osm_scanuploader_api:db(),
		displayfs=ep_osm_scanuploader:fs(displayfs),
		filterfs=[
			itf:build(?ITXPRF(profiletype), ?APPOSM_SCANUPLOADER),
			?ITXPRF(username),
			?ITXPRF(mobile),
			?ITXPRF(email)
		],
		size=10
	}.


%------------------------------------------------------------------------------
% renderers
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

