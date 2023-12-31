-module(ep_osm_apt_fields).
-compile(export_all).
-include("records.hrl").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------
f(apt_number = I) ->
	itf:textbox(?F(I, "Appointment Number"));


f(apt_state = I) ->
	itf:dropdown(?F(I, "Appointment State"), options(I));


f(evaluator_id = I) ->
	F = itf:textbox_picker(?F(I, "Evaluator")),
	F#field {options=options(I)};


f(evaluator_type = I) ->
	itf:dropdown(?F(I, "Evaluator Type"), options(I));


f(evaluator_state = I) ->
	itf:dropdown(?F(I, "Evaluator State"), options(I));


f(evaluator_fullname = I) ->
	F = itf:textbox(?F(I, "Evaluator Name")),
	F#field {
		doc=set,
		renderer=fun(_, _, #field {label=L, doc=Doc}) ->
			{ok, PDoc} = profiles:getdoc_from_cache(itf:val(Doc, evaluator_id)),
			{L, itf:val(PDoc, fullname)}
		end
	};

f(evaluator_mobile = I) ->
	F = itf:textbox(?F(I, "Evaluator Mobile")),
	F#field {
		doc=set,
		renderer=fun(_, _, #field {label=L, doc=Doc}) ->
			{ok, PDoc} = profiles:getdoc_from_cache(itf:val(Doc, evaluator_id)),
			{L, itf:val(PDoc, mobile)}
		end
	};

f(evaluator_email = I) ->
	F = itf:textbox(?F(I, "Evaluator Email")),
	F#field {
		doc=set,
		renderer=fun(_, _, #field {label=L, doc=Doc}) ->
			{ok, PDoc} = profiles:getdoc_from_cache(itf:val(Doc, evaluator_id)),
			{L, itf:val(PDoc, email)}
		end
	}.


%------------------------------------------------------------------------------
% validators
%------------------------------------------------------------------------------
validator(O) ->
	throw(O).

%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------


options(evaluator_id) ->
	#search {
		title=?LN("Select Evaluator"),
		db=profiles:getdb(),
		filterfs=[
			itf:dropdown(?F(profiletype, "Profile Type"), options(evaluator_type)),
			itf:textbox(?F(fullname)),
			itf:textbox(?F(mobile)),
			itf:textbox(?F(email))
		],
		displayfs=[
			fields:get(username),
			itf:textbox(?F(fullname)),
			itf:textbox(?F(mobile)),
			itf:textbox(?F(email))
		]
	};


options(evaluator_type) ->
	itf:options([
		?F(anpevaluator, "Evaluator"),
		?F(anpmoderator, "Moderator"),
		?F(anprevaluator, "Revaluator"),
		?F(anpmoderator_reval, "Reval-Moderator")
	]);


options(evaluator_state) ->
	itf:options([
		?F(noresponse, "No Response"),
		?F(accepted, "Accepted"),
		?F(rejected, "Rejected")
	]);


options(apt_state) ->
	itf:options([
		?F(new, "New"),
		?F(sent, "Sent"),
		?F(discarded, "Discarded")
	]).

%------------------------------------------------------------------------------
% renderers
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

