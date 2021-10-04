
-module(dig_ep_osm_exam).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("OSM Exams").

heading() ->
	title().

form() ->
	ep_osm_exam.


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=anptest:fs(search),
		size=25,
		config=[
			{responsive_type, collapse}
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exams").



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
% []
%
%..............................................................................
fetch(D, From, Size, Fs) ->

	%
	% fetch documents from db
	%
	Rec = db2_find:getrecord_by_fs(anptests:getdb(), Fs, From, Size),
	#db2_find_response {docs=Docs}  = db2_find:find(
		Rec#db2_find {sort=anptest:fs(search)}
	),

	%
	% layout results
	%
	Results = lists:map(fun(Doc) ->

		%
		% layout cells
		%
		FsDoc = itf:d2f(Doc, anptest:fs(form)),
		FsIndex = itf:d2f(Doc, anptest:fs(index)),
		lists:map(fun(F) ->
			#dcell {val=itl:render(F)}
		end, FsDoc) ++ [
			#dcell {val=layout_files(Doc)}
		] ++ [
			#dcell {val=helper_ui:layout_slinks(anptest, FsIndex)}
		]

	end, Docs),


	%
	% header
	%
	Header = lists:map(fun(#field {label=Label}) ->
		#dcell {type=header, val=Label}
	end, anptest:fs(form)) ++ [
		#dcell {type=header, val="Files"},
		#dcell {type=header, val="Actions"}

	],

	{
		D#dig {
			total=?INFINITY,
			actions=[
				{action_import, "+ Import", "+ Import"},
				{action_uploadzip, "Upload Zip", "Upload Zip"}
			]
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
	handle_objectkey(wf:q(objectkey)),
	dig:dig(?MODULE:get()).



%..............................................................................
%
% layout - files
%
%..............................................................................

layout_files(Doc) ->

	%
	% init
	%
	Names = attachment:get_names(Doc),


	%
	% layout
	%
	lists:map(fun(Name) ->
		#button {
			style="display:block;",
			class="btn btn-link",
			text=Name,
			postback={download, itf:idval(Doc), Name}
		}
	end, Names).




%..............................................................................
%
% layout - upload form
%
%..............................................................................

layout_upload_form() ->

	%
	% init
	%
	SeasonId = wf:q(season_fk),
	?ASSERT(
		((SeasonId /= []) and (SeasonId /= undefined)),
		"ERROR: Please select a season under which files are to be uploaded"
	),


	ObjectKey = ?FLATTEN(io_lib:format("~s_~s.zip", [
		SeasonId,
		helper:uidintstr()
	])),

	RedirectUrl = ?FLATTEN(io_lib:format("~s/~p?objectkey=~s", [
		customer:get(mainserver_url),
		?MODULE,
		ObjectKey
	])),

	Es = [
		itxfile_s3_upload:form([
			configs:get(aws_s3_bucket, []),
			configs:get(aws_s3_access_key, []),
			configs:get(aws_s3_secret, []),
			configs:get(aws_s3_default_region, []),
			ObjectKey,
			RedirectUrl
		])
	],
	[
		itl:instructions([
			{danger, "Folder name and zip file name should be same. Ex: Folder: 200, Zip: 200.zip"},
			{ok, "Zip file should contain folders named by course ID"},
			{ok, "Smaller PDF is assumed to be question paper and the bigger file is model answer."},
			{danger, "Existing files will be overwritten!"}
		]),
		Es
	].


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({browser_to_s3_completed, ObjectKey}) ->
	handle_objectkey_upload_completed(ObjectKey);

event(action_uploadzip) ->
	handle_action_uploadzip();

event({download, DocId, AttachmentName}) ->
	attachment:download(anptests:getdb(), DocId, AttachmentName);

event(E) ->
	dig_mm:event(E).

start_upload_event(Event) ->
	dig_mm:start_upload_event(Event).

finish_upload_event(Tag, AttachmentName, LocalFileData, Node) ->
	dig_mm:finish_upload_event(Tag, AttachmentName, LocalFileData, Node).


%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - object key upload completed
%
%..............................................................................

handle_objectkey_upload_completed(ObjectKey) ->
	dig_ep_osm_exam_file_upload:upload(ObjectKey).


%..............................................................................
%
% handle - object key
%
%..............................................................................


handle_objectkey(ObjectKey) when ObjectKey /=[], ObjectKey /= undefined ->
	%
	% before processing check if object exists
	%
	try
		_Infos = helper_s3:info_dir(
			configs:get(aws_s3_bucket, []),
			"browser_to_s3/" ++ ObjectKey
		),

		wf:wire(#event{type=timer, delay=100, postback={browser_to_s3_completed, ObjectKey}}),
		#span {text=[]}

	catch
		_E:_M ->
			skip
	end;

handle_objectkey(_) ->
	ok.



%..............................................................................
%
% handle - action upload zip
%
%..............................................................................

handle_action_uploadzip() ->
	%
	% build header
	%
	EsHeader = [
		#button {
			class="btn btn-sm btn-primary-outline pull-sm-right",
			text="Close",
			actions=[
				#event {
					type=click,
					actions=#update {target=panel_actions, elements=[]}
				}
			]
		},
		#p {
			class="font-weight-bold",
			text="Upload Zip"
		}
	],


	%
	% build form
	%
	Es = [
		layout_upload_form()
	],


	%
	% show form
	%
	Es1 = itl:section(EsHeader, Es),
	wf:update(panel_actions, Es1).




%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
