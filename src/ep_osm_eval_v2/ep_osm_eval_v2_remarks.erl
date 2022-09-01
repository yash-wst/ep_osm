-module(ep_osm_eval_v2_remarks).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



%-----------------------------------------------------------------------------------------------
%
% panel - remarks
%
%-----------------------------------------------------------------------------------------------
layout_remarks_panel(Fs) ->
	#panel{
		class="text-start p-5",
		body=[

		#textarea {
			id=txtarea_remarks_id,
			style="border:none;",
			class="form-control",
			placeholder="Write your remarks here...",
			text=""
		},

		#hr{
			 style="height:1px;",
			 class="mx-0 my-3"
		},

		ite:button(
			btn_add_remarks,
			"Add Remark",
			{add_remark},
			"btn btn-primary"
		),

		#hr{},

		#table {
			class="table table-bordered table-hover table-sm",
			%
			% layout all comment rows
			%
			rows= lists:map(fun({K, V}) ->
				[Date, Time, IP, User] = case string:tokens(K, " ") of
					[D,T,I,U] -> [D,T,I,U];
					[D,T,I,U,U1] -> [D,T,I,U++" "++U1] % patch - space in username
				end,
				#tablerow {cells=[layout_remark(Date, Time, IP, User, V)]}
			end, lists:reverse(fields:getuivalue(Fs, comments)))
		}
		]
	}.




%------------------------------------------------------------------------------
%
% layout remarks
%
%------------------------------------------------------------------------------
layout_remark(Date, Time, IP, Username, Message) ->
	#panel{
		body=[
			#hr{
			 style="height:1px;border:none;margin:0;",
			 class="mt-3"
			},

			#panel{
				style="font-size:12px",
				class="text-secondary fw-light mt-3",
				text=itx:format("~ts   ~ts   ~ts   ~ts", [Date, Time, IP, Username])
			},
			#panel{
				class="text-dark mt-1",
				body=Message
			}
		]
	}.

layout_remarks_update(Fs) ->
	wf:update(anpcandidate_remarks, layout_remarks_panel(Fs)).
