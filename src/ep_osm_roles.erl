-module(ep_osm_roles).
-compile(export_all).
-include("records.hrl").

%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

roles() -> [
	#itxrole {
		id=?APPOSM_ADMIN,
		name="Admin",
		description="UniApps Admin",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_PHYSICAL_INWARDER,
		name="Physical Inwarder",
		description="Create bundles in the system after 
			receiving packets from the university.",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_RECEIVER,
		name="OSM Receiver",
		description="Create entries for all booklets in a bundle.",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_SCANUPLOADER,
		name="Scanner / Uploader",
		description="Scan and upload images into the system",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_CONTROLLER,
		name="OSM Controller",
		description="Controller account to monitor project status.",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_QC,
		name="Quantity Checker",
		description="Role to check quality of scanned images.",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_CAPADMIN,
		name="CAP Centre Admin",
		description="Monitor project status at the centre",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_ANPADMIN,
		name="OSM Admin",
		description="OSM Admin has full access to OSM backend modules such as 
			content, results, etc.",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_EVALUATOR,
		name="OSM Evaluator",
		description="OSM evaluator does the initial evaluation of booklets",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_MODERATOR,
		name="OSM Moderator",
		description="OSM moderator does the second round evaluation of booklets",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_REVALUATOR,
		name="OSM Revaluator",
		description="Role to evaluate booklets that appear for re-valuation",
		app=ep_osm,
		subdomain="osm"
	},
	#itxrole {
		id=?APPOSM_MODERATOR_REVAL,
		name="OSM Moderator-Reval",
		description="Second level evaluation of re-valuation booklets",
		app=ep_osm,
		subdomain="osm"
	}
].


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
