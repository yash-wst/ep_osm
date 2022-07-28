-module (ep_osm_web_landing).
-compile(export_all).
-include("records.hrl").
-include_lib("itx/include/records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	?AKIT(#template {file="lib/itx/priv/static/templates/html/entry.html"}).

title() ->
	"Examination Processes - On Screen Marking System".

heading() ->
	"Examination Processes - On Screen Marking System".

layout() ->
	[
		layout_nav(),
		layout:grow(layout_body()),
		layout:grow(#p {style="margin-top: 300px;", text=[]})
	].

layout_nav() ->
	[
		#panel {class="mycenter", body=[
			#link {class="btn btn-link", text="Home", url="/ep_osm_web_landing"},
			#link {class="btn btn-link", text="Admin Login", url="/login"},
			#link {class="btn btn-link", text="Evaluator Login", url="/login"},
			#link {class="btn btn-link", text="Controller Login", url="/itxlogin"},
			#link {class="btn btn-link", text="CAP Centre Login", url="/itxlogin"},
			#link {class="btn btn-link", text="DTP Login", url="/itxlogin"}
		]}
    ].

layout_body() ->
	#panel {
		style="padding-top: 2rem;",
		body=[
			layout:g(8, 2, body())
		]
	}.



body() ->
	body(itxconfigs_cache:get2(ep_osm_landing_text, []), itxauth:role()).


body(Html, ?ADMIN) ->
	F = itf:build(itf:html(?F(ep_osm_landing_text, "Landing Page")), Html),
	itl:get(?EDIT, [F], ite:get(edit), simple);

body([], _) ->
	itl:section(#p {
		class="mycenter",
		text="Welcome to On-Screen Marking System"
	});
body(Html, _) ->
	itl:section(Html).


event(edit) ->
	Html = wf:q(ep_osm_landing_text),
	{ok, _} = itxconfigs:save({ep_osm_landing_text, Html, html, active}),
	helper_ui:flash(success, "Updated");


event({itx, E}) ->
	ite:event(E).
