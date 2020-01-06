
-module(dig_ep_osm_mscheme).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("Marking Scheme").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=[
			?OSMMSC(name),
			?OSMMSC(state)
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("Marking Scheme").



%------------------------------------------------------------------------------
% function - init
%------------------------------------------------------------------------------
init() ->
	ok.


%------------------------------------------------------------------------------
% function - fetch
%------------------------------------------------------------------------------

%..............................................................................
%
% [_]
%
%..............................................................................
fetch(D, From, Size, Fs) ->

	%
	% get docs
	%
	#db2_find_response {docs=Docs} = db2es_find:get_by_fs(
		ep_osm_mscheme_api:db(), Fs, From, Size
	),


	%
	% layout
	%
	Results = lists:map(fun(Doc) ->
		[
			#dcell {val=itf:val(Doc, name)},
			#dcell {val=helper:titlecase(itf:val(Doc, state))},
			#dcell {val=#link {
				text="Edit",
				url=io_lib:format("/ep_osm_mscheme?mode=edit&id=~s", [itf:idval(Doc)])
			}},
			#dcell {
				val=ite:button(clone, "Clone", {clone, itf:idval(Doc)})
			}
		]
	end, Docs),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Name"},
		#dcell {type=header, val="State"},
		#dcell {type=header, val="Edit"},
		#dcell {type=header, val="Clone"}
	],


	%
	% return
	%
	{
		D#dig {
			total=?INFINITY
		},
		[Header] ++ Results
	}.


%------------------------------------------------------------------------------
% function - exports
%------------------------------------------------------------------------------
exports() -> [
].



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	dig:dig(?MODULE:get()).



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({confirmation_yes, {clone, Id}}) ->
	handle_clone_confirmed(Id);

event({clone, Id}) ->

	{ok, Doc} = ep_osm_mscheme_api:get(Id),
	itl:confirmation(
		#panel {class="mycenter", body=[
			#p {text="Are you sure you want to clone?"},
			#p {text=io_lib:format("~s, ~s", [itf:val(Doc, name), itf:val(Doc, state)])}
		]},
		{clone, Id}
	);

event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------

%
% handle - clone confirmed
%
handle_clone_confirmed(Id) ->

	%
	% init
	%
	{ok, Doc} = ep_osm_mscheme_api:get(Id),


	%
	% build fs
	%
	NewName = io_lib:format("~s (~s)", [
		itf:val(Doc, name), helper:epochtimetostring(helper:epochtime())
	]),
	FsToSave = [
		itf:build(?OSMMSC(name), NewName),
		itf:build(?OSMMSC(state), "draft"),
		itf:d2f(Doc, ?OSMMSC({list_of_widgets, list_of_widgets}))
	],


	%
	% save
	%
	case ep_osm_mscheme_api:save(FsToSave) of
		{ok, NewDoc} ->
			Uri = io_lib:format("/ep_osm_mscheme?mode=edit&id=~s", [itf:idval(NewDoc)]),
			helper:redirect(Uri);
		_Error ->
			helper_ui:flash(error, "failed!", 5)
	end.



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
