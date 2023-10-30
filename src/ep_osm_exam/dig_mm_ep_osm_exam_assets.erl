-module(dig_mm_ep_osm_exam_assets).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------
main() ->
    ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).
title() ->
    ?LN("Exam assets").
heading() ->
    title().
form() ->
    ep_osm_exam.

%------------------------------------------------------------------------------
% fs
%------------------------------------------------------------------------------
fs(search) ->
	Fids =[
	season_fk,
	faculty_code_fk,
	program_code_fk,
	subject_code_fk,
	anptestcourseid,
	testname,
	teststatus,
	osm_mscheme_fk
	],
	fields:getfields(Fids).

%------------------------------------------------------------------------------
% fs - group
%------------------------------------------------------------------------------
fields(ep_osm_exam_assets, _Fs) ->
    fs(search).

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?ADMIN) -> true;
access(_, _) -> false.

%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------
get() ->
    #dig {
        module=?MODULE,
        filters=fs(search),
        size=25
    }.

%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
    ?LN("Exam assets").

%------------------------------------------------------------------------------
% function - init
%------------------------------------------------------------------------------
init() ->
    ok.

%------------------------------------------------------------------------------
% function - fetch
%------------------------------------------------------------------------------
fetch(D, _From, _Size, []) ->
    {D, {custom, itl:section(#p {class="mycenter", text="Please select at least one filter"})}};
fetch(D, From, Size, Fs) ->
    Docs = ep_osm_exam_api:fetch(From, Size, Fs),
    SubjectIds = lists:map(fun(Edoc) ->
        itf:val(Edoc, subject_code_fk)
    end, Docs),
    SubjectDocs = ep_core_subject_api:getdocs_by_ids(SubjectIds),
    SubjectDocsDict = helper:get_dict_from_docs(SubjectDocs),
    Results = lists:map(fun(Doc) ->
        {ok, SubDoc} = dict:find(itf:val(Doc, subject_code_fk), SubjectDocsDict),
        [
            #dcell{val=itf:val(SubDoc, subject_code)},
            #dcell{val=itf:val(Doc, testname)},
            #dcell{val=itf:val(Doc, teststatus)},
            layout_marking_scheme(Doc),
            layout_exam_attachments_status(Doc, questionpaper),
            layout_exam_attachments_status(Doc, modelanswers),
            #dcell{val=layout_attachments(Doc)},
			#dcell {
                val=akit_dropdown:button_group(
                        "Actions",
                        layout_actions(itf:idval(Doc)),
                        "btn-sm btn-primary-outline btn-outline-primary"
                    )
			}
        ]
    end, Docs),
    {
        D,
        [layout_header() | Results]
    }.

layout_attachments(Doc) ->
    Es = itl:get(?EDIT, [itf:attachment(?F({itf:idval(Doc), itf:revval(Doc)}, "File"))], noevent, box),
    Files = dig_ep_osm_exam:layout_files(Doc),
    #panel{
        body=[
            #p{body=Files},
            #p{body=Es}
        ]
    }.

%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
    dig_mm:layout(?MODULE).


layout_header() -> [
    #dcell{type=header, val="Subject Code"},
    #dcell{type=header, val="Test Name"},
    #dcell{type=header, val="Status"},
    #dcell{type=header, val="Marking Scheme"},
    #dcell{type=header, val="Question Paper"},
    #dcell{type=header, val="Model Answer"},
    #dcell{type=header, val="Upload"},
	#dcell {type=header, show_csv=false, val="Actions"}
].


layout_actions(Id) -> [
    #link{url=io_lib:format("/anptest?mode=view&anptestid=~s", [Id]), text="View", new=true},
    #link{url=io_lib:format("/anptest?mode=edit&anptestid=~s", [Id]), text="Edit", new=true}
].

%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event(E) ->
    dig_mm:event(E).
start_upload_event(Event) ->
    dig_mm:start_upload_event(Event).

finish_upload_event({attachment_upload1, {ExamId, Rev}}, AttachmentName, LocalFileData, Node) ->
    Fields = [
        fields:build(anptestid, ExamId),
        fields:build('_rev', Rev)
    ],
    anptests:attachment_upload(Fields, AttachmentName, LocalFileData),
    helper:redirect(wf:uri());

finish_upload_event(Tag, AttachmentName, LocalFileData, Node) ->
    anptest:finish_upload_event(Tag, AttachmentName, LocalFileData, Node).

%
% override before save function
%
before_save(FsToSave, _FsAll, _Doc) ->
    FsToSave.

%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------
layout_exam_attachments_status(EDoc, Type) ->
    AttachmentNames = attachment:get_names(anptests:getdb(), itf:idval(EDoc)),
    PdfName = itx:format("~s_~p.pdf", [itf:val(EDoc, anptestcourseid), Type]),
    case lists:member(PdfName, AttachmentNames) of
        false ->
            #dcell{
                val="Not available",
                bgcolor="table-danger"
            };
        _ ->
            #dcell{
                val="Available",
                bgcolor="table-success"
            }
    end.


layout_marking_scheme(Doc) ->
    case itf:val(Doc, osm_mscheme_fk) of
        [] ->
            #dcell{
                val="Not available",
                bgcolor="table-danger"
            };
        _MarkingScheme ->
            #dcell{
                val="Available",
                bgcolor="table-success"
            }
    end.

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------