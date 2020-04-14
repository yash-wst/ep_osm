-module(ep_osm_eval).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/ep_osm/priv/static/templates/html/ep_osm_eval.html"}).

title() ->
	?LN("Evaluation").

heading() ->
	?LN("Evaluation").

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------

access(_, ?APPOSM_EVALUATOR) -> true;
access(_, ?APPOSM_MODERATOR) -> true;
access(_, ?APPOSM_REVALUATOR) -> true;
access(_, ?APPOSM_REVALUATOR) -> true;
access(_, ?APPOSM_MODERATOR_REVAL) -> true;
access(_, _) -> false.



%-----------------------------------------------------------------------------------------------
% nav
%-----------------------------------------------------------------------------------------------
nav() ->
	[
		#panel {
			class="pull-sm-left",
			body=#link {
				url="/",
				body="<i class='fa fa-home fa-2x' aria-hidden='true'></i>"
			}
		},
		#panel {
			class="pull-sm-right",
			body=identity()
		},
		#panel {
			class="mycenter",
			body=anpcandidate:layout_review_nav()
		}
	].


%-----------------------------------------------------------------------------------------------
% layout
%-----------------------------------------------------------------------------------------------
layout() ->

	TId = wf:q(anptest:id()),
	TFs = anptests:get(TId),
	Fs = anpcandidates:get(anpcandidate:db(), wf:q(anpcandidate:id())),

	Elements = [
		#panel {
			style="margin: 0px;",
			class="mscheme-text",
			body=string:join(lists:duplicate(5, " MARKING SCHEME "), " / ")
		},
		#panel {
			style="border: 1px solid #fff; box-shadow: 5px 0 5px -1px #888;",
			html_id="sidebar-wrapper",
			body=anpcandidate:layout_review_right(TFs, Fs)
		},
		#panel {
			style="padding-top: 60px; padding-left: 2.2em;",
			html_id="page-content-wrapper",
			body=layout:grow([
				anpcandidate:layout_review_left(TFs, Fs)
			])
		}
	],

	wf:wire(#api {name=canvas_save, tag=canvas_save}),

	Elements.



%-----------------------------------------------------------------------------------------------
% event
%-----------------------------------------------------------------------------------------------
event(E) ->
	anpcandidate:event(E).

api_event(X, Y, Z) ->
	anpcandidate:api_event(X, Y, Z).




%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

identity() ->
	User = wf:user(),
	FullName = case itxauth:fullname() of
		undefined -> User;
		[] -> User;
		_ -> itxauth:fullname()
	end,
	[
		#p {
			class="text-uppercase",
			style="font-size: 0.8em; margin: 0px; font-weight: bold; word-wrap: break-word;",
			text=FullName
		},
	 	#p {style="font-size: 0.8em; margin: 0px;", body=[
	 		#span {
	 			text=io_lib:format("(~s) ", [itxauth:user()])
	 		},
	 		#span {
	 			text=itxauth:email()
	 		}
	 	]}
	].


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
