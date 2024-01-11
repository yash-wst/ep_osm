-module(ep_osm_mscheme_fields).
-compile(export_all).
-include("records.hrl").
-include("records_ep_osm_mscheme.hrl").


%------------------------------------------------------------------------------
% fields - template
%------------------------------------------------------------------------------

f(name = I) ->
	itf:textbox(?F(I, "Name"));

f(state = I) ->
	itf:dropdown(?F(I), options(I));

f(list_of_widgets = I) ->
	itf:subfields(?F(I, "Marking Scheme"), [
	]);

f(osm_mscheme_fk = I) ->
	F = itf:textbox_picker(?F(I, "Marking Scheme", test_settings)),
	F#field {
		module=ep_osm_mscheme,
		validators=[],
		options=options(I)
	};

f(list_of_export_markers = I) ->
	itf:list2(?F(I, "Export Markers"));

f(notes = I) ->
	itf:notes(?F(I, "Notes"));


%------------------------------------------------------------------------------
% fields - widget
%------------------------------------------------------------------------------

f({wtype, I}) ->
	itf:hidden(?F(I, "Widget Type"));

f({wid, I}) ->
	F = itf:textbox_readonly(?F(I, "Widget Id")),
	F#field {renderer=fun renderer_wid/3};

f({wname, I}) ->
	f({wname, I, undefined});

f({wname, I, WType}) when
	WType == ?WTYPE_QUESTION;
	WType == ?WTYPE_GROUP;
	WType == ?WTYPE_RULE ->
	itf:textbox(?F(I, "Name"));
f({wname, I, _WType})  ->
	itf:textbox_readonly(?F(I, "Name"));


f({wmarks, I}) ->
	f({wmarks, I, undefined});

f({wmarks, I, WType}) when WType == ?WTYPE_QUESTION ->
	itf:textbox(?F(I, "Marks"));
f({wmarks, I, _WType}) ->
	itf:hidden(?F(I, "Marks"));


f({list_of_widgets, I}) ->

	%
	% init - field
	%
	F = itf:subfields(?F(I, "Marking Scheme"), []),


	%
	% set loader function
	%
	F#field {
		subfields_loadfn=fun(Doc) ->
			case Doc of
				undefined ->
					[];
				{[]} ->
					[];
				{DocVals} ->
					lists:map(fun({WidgetId, {WidgetSubfields}}) ->
						[{_, WType}, {_, WId} | _] = WidgetSubfields,
						itf:d2f(Doc, ?OSMMSC({
							widget, helper:b2a(WidgetId), ?B2L(WType), ?B2L(WId)
						}))
					end, DocVals)
			end
		end
	};


%------------------------------------------------------------------------------
% fields - widget types
%------------------------------------------------------------------------------


%..............................................................................
%
% widget - insert
%
%..............................................................................

f({widget, WUId, ?WTYPE_INSERT, ?WID_INSERT}) ->
	f({widget, WUId, ?WTYPE_INSERT, ?WID_INSERT, ?WNAME_INSERT, [], []});



%..............................................................................
%
% widget - question
%
%..............................................................................

f({widget, WUId, ?WTYPE_QUESTION, ?WID_QUESTION}) ->
	f({widget, WUId, ?WTYPE_QUESTION, ?WID_QUESTION, [], marks_per_question(), []});



%..............................................................................
%
% widget - group
%
%..............................................................................

f({widget, WUId, ?WTYPE_GROUP, ChildrenCountStr}) when is_list(ChildrenCountStr) ->
	f({widget, WUId, ?WTYPE_GROUP, ?S2I(ChildrenCountStr)});
f({widget, WUId, ?WTYPE_GROUP, ChildrenCount}) ->

	%
	% init
	%
	ChildrenCountStr = ?I2S(ChildrenCount),


	%
	% build child elements
	%
	GroupChildren = lists:map(fun(I) ->
		f({widget, nid(WUId, ?I2A(I)), ?WTYPE_QUESTION, ?WID_QUESTION})
	end, lists:seq(1, ChildrenCount)),


	%
	% return
	%
	f({widget, WUId, ?WTYPE_GROUP, ChildrenCountStr, ?WNAME_GROUP, [], GroupChildren});




%..............................................................................
%
% widget - rule: or
%
%..............................................................................

f({widget, WUId, ?WTYPE_RULE, ?WID_OR}) ->

	%
	% build child elements
	%
	GroupChildren = [
		f({widget, nid(WUId, '1'), ?WTYPE_QUESTION, ?WID_QUESTION}),
		f({widget, nid(WUId, '2'), ?WTYPE_QUESTION, ?WID_QUESTION})
	],


	%
	% return
	%
	f({widget, WUId, ?WTYPE_RULE, ?WID_OR, ?WNAME_OR, [], GroupChildren});



%..............................................................................
%
% widget - rule
%
%..............................................................................

f({widget, WUId, ?WTYPE_RULE, ?WID_ANY ++ ChildrenCountStr}) ->
	f({widget, WUId, ?WTYPE_RULE, {?S2I(ChildrenCountStr), 0}});

f({widget, WUId, ?WTYPE_RULE, {AnyI, OfJ}}) ->

	%
	% init
	%
	AnyIStr = ?I2S(AnyI),
	WidgetId = ?WID_ANY ++ AnyIStr,
	WidgetName = io_lib:format("Any ~p of ~p", [AnyI, OfJ]),


	%
	% build child elements
	%
	GroupChildren = lists:map(fun(Ji) ->
		f({widget, nid(WUId, ?I2A(Ji)), ?WTYPE_QUESTION, ?WID_QUESTION})
	end, lists:seq(1, OfJ)),


	%
	% return
	%
	f({widget, WUId, ?WTYPE_RULE, WidgetId, WidgetName, [], GroupChildren});



%..............................................................................
%
% widget - impl
%
%..............................................................................

f({widget, WUId, WType, WId, WName0, WMarks, WChildren}) ->


	%
	% sub fields of this widget
	%
	WName = case WName0 of
		[] ->
			get_question_id_from_wuid(WUId);
		_ ->
			WName0
	end,
	Subfields = [
		itf:build(?OSMMSC({wtype, nid(WUId, wtype)}), WType),
		itf:build(?OSMMSC({wid, nid(WUId, wid)}), WId),
		itf:build(?OSMMSC({wname, nid(WUId, wname), WType}), WName),
		itf:build(?OSMMSC({wmarks, nid(WUId, wmarks), WType}), WMarks),
		itf:build(?OSMMSC({list_of_widgets, nid(WUId, list_of_widgets)}), WChildren)
	],


	%
	% return widget
	%
	#field {
		id=WUId,
		baseid=WUId,
		type=subfields,
		label=WName,
		subfields=Subfields,
		renderer=renderer({WUId, WType, WId}),
		subfields_loadfn=fun(WidgetDoc) ->
			case WidgetDoc of
				undefined ->
					[];
				_ ->
					itf:d2f(WidgetDoc, Subfields)
			end
		end
	};



f(O) ->
	throw(O).

%------------------------------------------------------------------------------
% validators
%------------------------------------------------------------------------------
validator(O) ->
	throw(O).

%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------

options(osm_mscheme_fk) ->
	#search {
		title=?LN("Select Marking Scheme"),
		db=ep_osm_mscheme_api:db(),
		displayfs=ep_osm_mscheme:fs(displayfs),
		filterfs=[
			f(name),
			f(state)
		],
		size=10
	};


options(state) ->
	itf:options([
		?F(draft, "Draft"),
		?F(published, "Published"),
		?F(discarded, "Discarded")
	]).

%------------------------------------------------------------------------------
% renderers
%------------------------------------------------------------------------------

%..............................................................................
%
% renderer - question
%
%..............................................................................

renderer({WUId, ?WTYPE_QUESTION, _}) ->
	fun(Mode, _Event, #field {subfields=Subfields} = F) ->

		%
		% render button
		%
		[FWType, FWId, FWname, FWmarks, FWLow] = Subfields,

		EsVisible = layout:grow([
			layout:g(3, layout_wuid(WUId)),
			layout:g(3, itl:render(Mode, FWname)),
			layout:g(3, itl:render(Mode, FWmarks)),
			layout:g(3, layout_actions(WUId, F))
		]),
		dig_mm:wire_page_validation(Mode, edit, [FWname, FWmarks], Mode == ?EDIT),


		%
		% render hidden fields
		%
		EsHiddenFields = #panel {
			style="display: none;",
			body=lists:map(fun(Fi) ->
				itl:render(Mode, Fi)
			end, [FWType, FWId, FWLow])
		},


		%
		% return
		%
		{
			nolabel,
			[
				EsVisible,
				EsHiddenFields
			]
		}
	end;



%..............................................................................
%
% renderer - insert
%
%..............................................................................

renderer({WUId, ?WTYPE_INSERT, _}) ->
	fun(Mode, _Event, #field {subfields=Subfields} = F) ->

		%
		% render button
		%
		EsButton = #button {
			class="btn btn-danger",
			text="Pick",
			delegate=?MODULE,
			postback={insert, WUId, F}
		},

		%
		% render subfields
		%
		EsSubFields = #panel {
			style="display: none;",
			body=lists:map(fun(Fi) ->
				itl:render(Mode, Fi)
			end, Subfields)
		},


		%
		% return
		%
		{
			nolabel,
			[EsButton, EsSubFields]
		}
	end;



%..............................................................................
%
% renderer - rule: or
%
%..............................................................................

renderer({WUId, ?WTYPE_RULE, ?WID_OR}) ->
	fun(Mode, _Event, #field {subfields=Subfields} = F) ->

		%
		% init
		%
		[_FWType, FWId, FWname, _FWmarks, FWLow] = Subfields,
		Separator =	#p {class="font-weight-bold mycenter", text="(OR)"},


		%
		% render or fields
		%
		EsFields = lists:map(fun(Fi) ->
			itl:render(Mode, Fi)
		end, FWLow#field.subfields),


		{EsClass, EsText} = case length(EsFields) of
			1 ->
				{"text-danger myitalic", "Error: An 'OR' rule needs at least 2 questions"};
			_ ->
				{"", "."}
		end,


		%
		% render subfields
		%
		EsSubFields = layout:grow([
				layout:g(3, layout_wuid(WUId)),
				layout:g(3, itl:render(Mode, FWname)),
				layout:g(3, itl:render(Mode, FWId)),
				layout:g(3, layout_actions(WUId, F))
		]),


		%
		% return
		%
		{
			nolabel,
			section([
				EsSubFields,
				#p {class=EsClass, text=EsText},
				helper:join(EsFields, Separator)
			])
		}
	end;

%..............................................................................
%
% renderer - other
%
%..............................................................................

renderer({WUId, _, _}) ->
	fun(Mode, _Event, #field {subfields=Subfields} = F) ->


		[FWType, FWId, FWname, _FWmarks, FWLow] = Subfields,

		EsVisible = layout:grow([
			layout:g(3, layout_wuid(WUId)),
			layout:g(3, itl:render(Mode, FWname)),
			layout:g(3, itl:render(Mode, FWId)),
			layout:g(3, layout_actions(WUId, F))
		]),


		%
		% render hidden fields
		%
		EsHiddenFields = #panel {
			body=lists:map(fun(Fi) ->
				itl:render(Mode, Fi)
			end, [FWType, FWLow])
		},


		%
		% return
		%
		{
			nolabel,
			[
				EsVisible,
				EsHiddenFields
			]
		}
	end.


%..............................................................................
%
% renderer - wid
%
%..............................................................................

renderer_wid(_, _, #field {id=I, label=L, uivalue=Val}) ->
	
	ValLabel = case Val of
		"any" ++ _ ->
			string:to_upper(Val);
		_ ->
			[]
	end,

	{L, [
		#hidden {id=I, text=Val},
		#span {class="fw-bold", text=ValLabel}
	]}.




%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({confirmation_yes, {remove, F}}) ->
	ep_osm_mscheme_handler:handle_remove_widget(F);

event({remove, F}) ->
	itl:confirmation(
		"Are you sure you want to remove this widget?",
		{remove, F},
		?MODULE
	);

event({replace, WUId, #field {} = F}) ->
	itl:modal_fs(ep_osm_mscheme_layout:insert_buttons(WUId, F));

event({insert, WUId, #field {} = F}) ->
	itl:modal_fs(ep_osm_mscheme_layout:insert_buttons(WUId, F)).



%------------------------------------------------------------------------------
% layout
%------------------------------------------------------------------------------



%
% layout actions
%
layout_actions(WUId, F) -> [
	#button {
		class="btn btn-sm btn-danger-outline pull-sm-right",
		style="margin: 5px;",
		text="o",
		delegate=?MODULE,
		postback={replace, WUId, F}
	},
	#button {
		class="btn btn-sm btn-danger-outline pull-sm-right",
		style="margin: 5px;",
		text="x",
		delegate=?MODULE,
		postback={remove, F}
	}
].



%
% layout quid
%
layout_wuid(WUId) ->
	#span {
		class="font-weight-bold",
		body=[
			get_question_id_from_wuid(WUId),
			layout_wuid_marker_export(WUId)
		]
	}.



layout_wuid_marker_export(WUId) ->

	%
	% get doc and set value
	%
	Doc = ep_osm_mscheme:getdoc(),
	ExportMarkerList = itf:val(Doc, ?OSMMSC(list_of_export_markers)),
	IsChecked = proplists:get_value(?A2L(WUId), ExportMarkerList, false),


	#span {
		class="pull-sm-right float-end",
		body=[
			#checkbox {
				title="Show in results?",
				id=WUId,
				checked=IsChecked,
				postback={clicked, export_marker, WUId},
				delegate=ep_osm_mscheme_handler
			}
		]
	}.


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% nid
%
nid(A, B) when is_atom(B) ->
	nid(A, ?A2L(B));
nid(A, B) when is_list(B) ->
	?L2A(?A2L(A) ++ "_" ++ B).


%
% uid
%
uid() ->
	helper:uidintstr().

uid(Id) ->
	?L2A(?FLATTEN(io_lib:format("~s_~s", [Id, uid()]))).



%
% question id from quid
%
get_question_id_from_wuid(WUId) ->


	%
	% init
	%
	WUIdStr = ?A2L(WUId),
	Tokens = string:tokens(WUIdStr, "_"),


	%
	% return
	%
	WUIdStr1 = case Tokens of
		[First, Second | Rest] ->
			AlphebetIndex = ?S2I(Second),
			Alphabet = lists:nth(AlphebetIndex, helper:listof(alphabets)),
			io_lib:format("~s~s~s", [First, Alphabet, Rest]);
		_ ->
			WUIdStr
	end,
	string:to_upper(?FLATTEN("Q" ++ WUIdStr1)).



%
% marks_per_question
%
marks_per_question() ->
	try
		wf:q(marks_per_question)
	catch _:_ ->
		[]
	end.



%
% section
%
section(Es) ->
	#panel {class="it-section", body=Es}.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

