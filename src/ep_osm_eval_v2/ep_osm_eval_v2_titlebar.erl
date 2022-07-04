-module(ep_osm_eval_v2_titlebar).
-compile(export_all).
-include("records.hrl").

company_branding() ->
	[
		#panel{
			class="titlebar-col",
			body=[
				#link { body=#image {
								image="/lib/ep_osm/priv/static/images/logo.svg",
								class="uniapps-logo"
						},
						url = "/"
				},

				#span {text="UniApps", class="text-company-name"}
			]
		}
	].

user_identity() ->
	[
		#span { text=get_user_name(), class="text-username" }
	].

more_section() ->
	[
		#link {
			body=#panel {
					class="titlebar-img-arrow"
				}
		}
	].

right_section() ->
[
	#span{
		class="titlebar-col titlebar-col-right",
		body=[
				user_identity(),

				more_section()
		]
	}
].

get_user_name() ->
	User = wf:user(),
	FullName = case itxauth:fullname() of
		undefined -> User;
		[] -> User;
		_ -> itxauth:fullname()
		end,
	FullName.
