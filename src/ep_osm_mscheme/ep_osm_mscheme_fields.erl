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
	itf:subfields(?F(I, "Widget Children"), [
		f({widget, '1', ?WTYPE_INSERT, ?WID_INSERT}),
		f({widget, '2', ?WTYPE_INSERT, ?WID_INSERT}),
		f({widget, '3', ?WTYPE_INSERT, ?WID_INSERT})
	]);


%------------------------------------------------------------------------------
% fields - widget
%------------------------------------------------------------------------------

f({wtype, I}) ->
	itf:textbox(?F(I, "Widget Type"));

f({wid, I}) ->
	itf:textbox(?F(I, "Widget Id"));

f({wname, I}) ->
	itf:textbox(?F(I, "Widget Name"));

f({wmarks, I}) ->
	itf:textbox(?F(I, "Widget Marks"));

f({list_of_widgets, I}) ->

	%
	% init - field
	%
	F = itf:subfields(?F(I, "List of Widgets"), []),


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
	f({widget, WUId, ?WTYPE_QUESTION, ?WID_QUESTION, [], [], []});



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
		f({widget, ?NID(WUId, ?I2A(I)), ?WTYPE_QUESTION, ?WID_QUESTION})
	end, lists:seq(1, ChildrenCount)),


	%
	% return
	%
	f({widget, WUId, ?WTYPE_GROUP, ChildrenCountStr, ?WNAME_GROUP, [], GroupChildren});



%..............................................................................
%
% widget - rule
%
%..............................................................................

f({widget, WUId, ?WTYPE_RULE, ?WID_ANY ++ ChildrenCountStr}) when is_list(ChildrenCountStr) ->
	f({widget, WUId, ?WTYPE_RULE, ?S2I(ChildrenCountStr)});
f({widget, WUId, ?WTYPE_RULE, ChildrenCount}) ->

	%
	% init
	%
	ChildrenCountStr = ?I2S(ChildrenCount),
	WidgetId = ?WID_ANY ++ ChildrenCountStr,
	WidgetName = ?WNAME_ANY ++ ChildrenCountStr,


	%
	% build child elements
	%
	GroupChildren = lists:map(fun(I) ->
		f({widget, ?NID(WUId, ?I2A(I)), ?WTYPE_INSERT, ?WID_INSERT})
	end, lists:seq(1, ChildrenCount)),


	%
	% return
	%
	f({widget, WUId, ?WTYPE_RULE, WidgetId, WidgetName, [], GroupChildren});



%..............................................................................
%
% widget - impl
%
%..............................................................................

f({widget, WUId, WType, WId, WName, WMarks, WChildren}) ->


	%
	% sub fields of this widget
	%
	Subfields = [
		itf:build(?OSMMSC({wtype, ?NID(WUId, wtype)}), WType),
		itf:build(?OSMMSC({wid, ?NID(WUId, wid)}), WId),
		itf:build(?OSMMSC({wname, ?NID(WUId, wname)}), WName),
		itf:build(?OSMMSC({wmarks, ?NID(WUId, wmarks)}), WMarks),
		itf:build(?OSMMSC({list_of_widgets, ?NID(WUId, list_of_widgets)}), WChildren)
	],


	%
	% return widget
	%
	#field {
		id=WUId,
		baseid=WUId,
		type=subfields,
		label="Widget",
		subfields=Subfields,
		renderer=renderer({WUId, WType}),
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
% renderer - insert
%
%..............................................................................

renderer({WUId, ?WTYPE_INSERT}) ->
	fun(Mode, _Event, #field {subfields=Subfields} = F) ->

		%
		% render button
		%
		EsButton = #button {
			class="btn btn-sm btn-danger-outline",
			text="+",
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
% renderer - other
%
%..............................................................................

renderer(_) ->
	renderer_subfields_table:get().


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({insert, WUId, #field {} = F}) ->
	itl:modal_fs(ep_osm_mscheme_layout:insert_buttons(WUId, F)).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% uid
%
uid() ->
	helper:uidintstr().

uid(Id) ->
	?L2A(?FLATTEN(io_lib:format("~s_~s", [Id, uid()]))).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

