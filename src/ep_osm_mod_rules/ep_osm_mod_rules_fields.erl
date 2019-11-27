-module(ep_osm_mod_rules_fields).
-compile(export_all).
-include("records.hrl").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------
f(name = I) ->
	itf:textbox(?F(I, "Name"));


f(type = I) ->
	itf:dropdown(?F(I, "Type"), options(I));


f(frommarks = I) ->
	itf:textbox_int(?F(I, "From Marks"));


f(tomarks = I) ->
	itf:textbox_int(?F(I, "To Marks"));


f(movepercentage = I) ->
	itf:textbox_int(?F(I, "Move Percentage"));


f({FId, I}) ->
	?OSMRLS(FId, #field {id=I});


f(rules = I) ->

	%
	% define field
	%
	F = itf:field_group_list(
		{I, ?LN("Moderation Rules")},
		[
			frommarks, tomarks, movepercentage
		],
		{
			ep_osm_mod_rules,
			ep_osm_mod_rules_api,
			ep_osm_mod_rules_fields
		}
	),


	%
	% set renderer
	%
	F#field{
		renderer=renderer_subfields_table:get()
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
options(type) ->
	itf:options([
		?F(evaluation, "Apply to evaluators"),
		?F(moderation, "Apply to moderators"),
		?F(revaluation, "Apply to revaluators")
	]).

%------------------------------------------------------------------------------
% renderers
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

