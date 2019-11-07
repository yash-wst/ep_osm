-module(ep_osm_mscheme).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include("records_ep_osm_mscheme.hrl").

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Marking Scheme").

heading() ->
	?LN("OSM Marking Scheme").

db() ->
	ep_osm_mscheme_api:db().

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, _) -> false.

%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

fs(basic) -> [
	?OSMMSC(name),
	?OSMMSC(state),
	?OSMMSC(list_of_widgets)
];

fs(index) -> [
	?OSMMSC(name),
	?OSMMSC(state)
];

fs(view) ->
	fs(basic);

fs(create) -> [
	?OSMMSC(name)
];

fs(edit) -> [
	?OSMMSC({list_of_widgets, list_of_widgets})
];

fs(update) -> [
	?OSMMSC(name),
	?OSMMSC(state)
];


fs(search) ->
	fs(basic);

fs(grid) ->
	fs(basic);

fs(import) -> [
	itf:attachment()
];

fs(all) -> [
	itf:id(),
	itf:rev(),
	itf:attachment()
] ++
	fs(basic).

%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	layout(wf:q(mode), wf:q(id)).


%..............................................................................
%
% layout - create
%
%..............................................................................

layout(?CREATE, _) ->
	itl:get(?CREATE, fs(create), ite:get(create), table);



%..............................................................................
%
% layout - edit
%
%..............................................................................

layout(?EDIT, Id) when Id /= []; Id /= undefined ->


	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mscheme_api:get(Id),



	%
	% fs edit
	%
	FsEdit = case itf:d2f(Doc, ?OSMMSC({list_of_widgets, list_of_widgets})) of
		#field {subfields=[]} -> [
			?OSMMSC(list_of_widgets)
		];
		_ -> [
			?OSMMSC({list_of_widgets, list_of_widgets})
		]
	end,


	%
	% layout
	%
	Es = [
		layout_actions(),
		itl:get(?EDIT, itf:d2f(Doc, FsEdit), ite:get(edit), table)
	],
	layout:g(6, 3, Es);



%..............................................................................
%
% layout - other
%
%..............................................................................

layout(?UPDATE, Id) when Id /= []; Id /= undefined ->
	layout:g(6, 3, itxdocument:layout(?UPDATE, ?MODULE, Id));

layout(_, _) ->
	[].




%..............................................................................
%
% layout - actions
%
%..............................................................................

layout_actions() -> [
	#button {
		class="btn btn-sm btn-primary-outline",
		style="margin: 5px;",
		text="Add +",
		delegate=?MODULE,
		postback={action, add}
	},
	#button {
		class="btn btn-sm btn-primary-outline",
		style="margin: 5px;",
		text="Clear All x",
		delegate=?MODULE,
		postback={action, clearall}
	},
	#button {
		class="btn btn-sm btn-primary-outline pull-sm-right",
		style="margin: 5px;",
		text="View",
		delegate=?MODULE,
		postback={action, view_marking_scheme_layout}
	}
].


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({action, add}) ->
	handle_add();

event({action, view_marking_scheme_layout}) ->
	handle_view_marking_scheme_layout();

event({confirmation_yes, clearall}) ->
	handle_clearall();

event({action, clearall}) ->
	itl:confirmation(
		"Are you sure you want to clear this marking scheme?",
		clearall
	);

event(create) ->
	handle_create();

event(edit) ->
	handle_edit();

event(E) ->
	itxdocument:event(E, ?MODULE, wf:q(id)).


%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - add
%
%..............................................................................

handle_add() ->
	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mscheme_api:get(Id),
	#field {subfields=Subfields} = itf:d2f(Doc, ?OSMMSC({list_of_widgets, list_of_widgets})),


	%
	% get next subfield number
	%
	 NextSubfieldNumber = case Subfields of
	 	[] ->
	 		1;
	 	_ ->
	 		#field {id=LastSubfieldId} = lists:last(Subfields),
	 		?S2I(?A2L(LastSubfieldId)) + 1
	 end,


	 %
	 % show widgets
	 %
	itl:modal_fs(ep_osm_mscheme_layout:insert_buttons(?I2A(NextSubfieldNumber), undefined)).



%..............................................................................
%
% handle - convert doc to rules
%
%..............................................................................


handle_convert_doc_to_rules(Doc) ->
	FList = itf:d2f(Doc, ?OSMMSC({list_of_widgets, list_of_widgets})),
	handle_convert_widgets_to_tuples(undefined, FList#field.subfields).



handle_convert_widgets_to_tuples(ParentType, ListOfWidgets) ->
	lists:map(fun(W) ->
		handle_convert_widget_to_tuple(ParentType, W)
	end, ListOfWidgets).



handle_convert_widget_to_tuple(ParentType, #field {id=Id, subfields=[#field {uivalue=?WTYPE_QUESTION} | _] = Subfields}) ->
	[_FWType, _FWId, FWname, FWmarks, _FWLow] = Subfields,

	Tuple = {
		helper:replace_these_with_that(itf:val(FWname), [" "], "_"),
		helper:s2f_v1(itf:val(FWmarks))
	},

	TupleGroup = {
		group,
		?WTYPE_RULE ++ ?WTYPE_GROUP ++ ?WTYPE_QUESTION ++ ?A2L(Id),
		[Tuple]
	},

	TupleRule = {
		rule,
		"norule",
		?WTYPE_RULE ++ ?WTYPE_GROUP ++ ?A2L(Id),
		[TupleGroup]
	},

	case ParentType of
		undefined ->
			TupleRule;
		?WTYPE_RULE ->
			TupleGroup;
		?WTYPE_GROUP ->
			Tuple
	end;

handle_convert_widget_to_tuple(ParentType, #field {id=Id, subfields=[#field {uivalue=?WTYPE_GROUP} | _] = Subfields}) ->
	[_FWType, _FWId, _FWname, _FWmarks, FWLow] = Subfields,
	Tuple = {
		group,
		?WTYPE_GROUP ++ ?A2L(Id),
		handle_convert_widgets_to_tuples(?WTYPE_GROUP, FWLow#field.subfields)
	},
	case ParentType of
		?WTYPE_RULE ->
			Tuple;
		_ ->
			{
				rule,
				"norule",
				?WTYPE_RULE ++ ?WTYPE_GROUP ++ ?A2L(Id),
				[Tuple]
			}
	end;


handle_convert_widget_to_tuple(_, #field {id=Id, subfields=[#field {uivalue=?WTYPE_RULE} | _] = Subfields}) ->
	[_FWType, FWId, _FWname, _FWmarks, FWLow] = Subfields,
	{
		rule,
		itf:val(FWId),
		?WTYPE_RULE ++ ?A2L(Id),
		handle_convert_widgets_to_tuples(?WTYPE_RULE, FWLow#field.subfields)
	}.

%..............................................................................
%
% handle - view marking scheme layout
%
%..............................................................................

handle_view_marking_scheme_layout() ->


	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mscheme_api:get(Id),


	%
	% convert doc to rules
	%
	Rules = handle_convert_doc_to_rules(Doc),



	%
	% show
	%
	try
		helper:state(anpcandidate_state_marking, Rules),
		Es = anp_marking:layout_marking_rules(anpmarking_anpevaluator, []),
		itl:modal_fs(layout:grow(layout:g(4, 4, Es)))
	catch
		error:badarg ->
			helper_ui:flash(error, "Please enter all question numbers and marks correctly.", 5)
	end.



%..............................................................................
%
% handle - clearall
%
%..............................................................................

handle_clearall() ->

	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mscheme_api:get(Id),
	FList = itf:d2f(Doc, ?OSMMSC({list_of_widgets, list_of_widgets})),


	%
	% save
	%
	FsToSave = [
		FList#field {subfields=[]}
	],
	ep_osm_mscheme_handler:handle_save_and_reload(Id, FsToSave).


%..............................................................................
%
% handle - edit
%
%..............................................................................

handle_edit() ->

	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mscheme_api:get(Id),
	FsToSave = itf:uivalue(itf:d2f(Doc, fs(edit))),


	%
	% save
	%
	ep_osm_mscheme_handler:handle_save_and_reload(Id, FsToSave).


%..............................................................................
%
% handle - create
%
%..............................................................................

handle_create() ->

	%
	% init
	%
	FsUi = itf:uivalue(fs(create)),
	FsCreate = FsUi ++ [
		itf:build(?OSMMSC(state), "draft"),
		?OSMMSC(list_of_widgets)
	],


	%
	% save
	%
	case ep_osm_mscheme_api:save(FsCreate) of
		{ok, Doc} ->
			Url = io_lib:format("/~p?mode=edit&id=~s", [?MODULE, itf:idval(Doc)]),
			helper:redirect(Url);
		Error ->
			?D(Error),
			helper_ui:flash("Sorry, could not create!")
	end.


%------------------------------------------------------------------------------
% events - file
%------------------------------------------------------------------------------

start_upload_event(attachment_upload) ->
	itxdocument:start_upload_event(attachment_upload).

finish_upload_event(attachment_upload, AttachmentName, LocalFileData, Node) ->
	itxdocument:finish_upload_event(attachment_upload, AttachmentName, LocalFileData, Node).

%------------------------------------------------------------------------------
% links
%------------------------------------------------------------------------------
links() ->
	helper_ui:authorised_links(
		?MODULE,
		[?GRID, ?CREATE, ?SEARCH],
		itxauth:role(),
		undefined
	).

links(undefined) ->
	links();

links(Id) ->
	helper_ui:authorised_links(
		?MODULE,
		[?VIEW, ?EDIT],
		itxauth:role(),
		Id
	).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------
import(List) ->
	?D(List).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
