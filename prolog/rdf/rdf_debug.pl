:- module(
  rdf_debug,
  [
    rdf_store_messages/2, % +S, :Goal_0
    rdf_store_metadata/2, % +S, +M
    rdf_show_graph/1,     % +G
    rdf_show_graph/2      % +G, +Opts
  ]
).

/** <module> RDF debug tools

Show RDF data structures during modeling/development.

@author Wouter Beek
@version 2015/08, 2015/12-2016/03
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(gv/gv_file)).
:- use_module(library(http/json)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(msg_ext)).
:- use_module(library(option)).
:- use_module(library(os/gnu_wc)).
:- use_module(library(os/process_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(os/thread_counter)).
:- use_module(library(pl/pl_term)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_graph_viz)).
:- use_module(library(rdf/rdf_print_stmt)).
:- use_module(library(stream_ext)).
:- use_module(library(xml/xml_dom)).

:- meta_predicate
    rdf_store_messages(+, 0).

:- rdf_meta
   rdf_store(r, r, o),
   rdf_store(r, r, o, +),
   rdf_store_message(r, r, +),
   rdf_store_messages(r, :),
   rdf_show_graph(r),
   rdf_show_graph(r, +).

:- predicate_options(rdf_show_graph/2, 2, [
     pass_to(rdf_graph_to_export_graph/3, 3),
     pass_to(graph_viz/3, 3),
     pass_to(run_process/3, 3)
   ]).





%! rdf_store_message(+S, +P, +Term) is det.

% Archive error
rdf_store_message(S, P, error(archive_error(C,_),_)) :- !,
  (   C == 2
  ->  I = missingTypeKeywordInMtreeSpec
  ;   C == 25
  ->  I = invalidCentralDirectorySignature
  ),
  rdf_global_id(llo:I, O),
  rdf_store(S, P, O).

% Encoding: character
rdf_store_message(S, P, error(type_error(character,Char),context(_Pred,_Var))) :- !,
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, llo:'CharacterEncodingError'),
  rdf_store(B, llo:object, Char^^xsd:integer).

% Existence: directory
rdf_store_message(S, P, error(existence_error(directory,Dir),context(_Pred,Msg))) :- !,
  Msg == 'File exists',
  C0 = 'DirectoryExistenceError',
  rdf_global_id(llo:C0, C),
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, C),
  uri_file_name(Uri, Dir),
  rdf_store(B, llo:object, Uri^^xsd:anyURI).

% Existence: file
rdf_store_message(S, P, error(existence_error(file,File),context(_Pred,Msg))) :- !,
  (   Msg == 'Directory not empty'
  ->  C0 = 'DirectoryNotEmpty'
  ;   Msg == 'No such file or directory'
  ->  C0 = 'FileExistenceError'
  ),
  rdf_global_id(llo:C0, C),
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, C),
  uri_file_name(Uri, File),
  rdf_store(B, llo:object, Uri^^xsd:anyURI).

% Existence: source sink?
rdf_store_message(S, P, error(existence_error(source_sink,Path),context(_Pred,Msg))) :- !,
  Msg == 'Is a directory',
  C0 = 'IsADirectoryError',
  rdf_global_id(llo:C0, C),
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, C),
  uri_file_name(Uri, Path),
  rdf_store(B, llo:object, Uri^^xsd:anyURI).


% HTTP status
rdf_store_message(S, P, error(http_status(Status),_)) :- !,
  (   between(400, 499, Status)
  ->  rdf_store(S, P, llo:'4xxHttpError')
  ;   between(500, 599, Status)
  ->  rdf_store(S, P, llo:'5xxHttpError')
  ).

% IO: read
rdf_store_message(S, P, error(io_error(read,_Stream),context(_Pred,Msg))) :- !,
  (   Msg == 'Connection reset by peer'
  ->  I = connectionResetByPeer
  ;   Msg == 'Inappropriate ioctl for device'
  ->  I = notATypewriter
  ;   Msg = 'Is a directory'
  ->  I = isADirectory
  ),
  rdf_global_id(llo:I, O),
  rdf_store(S, P, O).

% IO: write
rdf_store_message(S, P, error(io_error(write,_Stream),context(_Pred,Msg))) :- !,
  Msg == 'Encoding cannot represent character',
  I = encodingError,
  rdf_global_id(llo:I, O),
  rdf_store(S, P, O).

% IO warning
rdf_store_message(S, P, io_warning(_Stream,Msg)) :- !,
  (   Msg == 'Illegal UTF-8 continuation'
  ->  I = illegalUtf8Continuation
  ;   Msg == 'Illegal UTF-8 start'
  ->  I = illegalUtf8Start
  ),
  rdf_global_id(llo:I, O),
  rdf_store(S, P, O).

% Malformed URL
rdf_store_message(S, P, error(domain_error(url,Url),_)) :- !,
  rdf_create_bnode(B),
  rdf_store(B, rdf:type, llo:'MalformedUrl'),
  rdf_store(B, llo:object, Url^^xsd:anyURI),
  rdf_store(S, P, B).

% No RDF
rdf_store_message(S, _P, error(no_rdf(_File))) :- !,
  rdf_store(S, llo:serializationFormat, llo:unrecognizedFormat).

% Permission: redirect
rdf_store_message(S, P, error(permission_error(Action0,Type0,Object),context(_,Msg1))) :- !,
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, llo:'PermissionError'),
  atom_truncate(Msg1, 500, Msg2),
  rdf_store(B, llo:message, Msg2^^xsd:string),

  % Action
  Action0 == redirect,
  rdf_global_id(llo:redirectAction, C),
  rdf_store(B, llo:action, C),

  % Object
  rdf_store(B, llo:object, Object),

  % Type
  Type0 == http,
  rdf_store(Object, llo:object, llo:'HttpUri').

% SGML parser
rdf_store_message(S, P, sgml(sgml_parser(_Parser),_File,Line,Msg1)) :- !,
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, llo:'SgmlParserError'),
  rdf_store(B, llo:sourceLine, Line^^xsd:nonNegativeInteger),
  atom_truncate(Msg1, 500, Msg2),
  rdf_store(B, llo:message, Msg2^^xsd:string).

% Socket error
rdf_store_message(S, P, error(socket_error(Msg),_)) :- !,
  (   Msg == 'Connection timed out'
  ->  I = connectionTimedOut
  ;   Msg == 'Connection refused'
  ->  I = connectionRefused
  ;   Msg == 'No Data'
  ->  I = noData
  ;   Msg == 'No route to host'
  ->  I = noRouteToHost
  ;   Msg == 'Host not found'
  ->  I = hostNotFound
  ;   Msg == 'Try Again'
  ->  I = tryAgain
  ),
  rdf_global_id(llo:I, O),
  rdf_store(S, P, O).

% SSL: SSL verify
rdf_store_message(S, P, error(ssl_error(ssl_verify),_)) :- !,
  rdf_store(S, P, llo:sslError).

% Syntax error
rdf_store_message(S, P, error(syntax_error(Msg1),stream(_Stream,Line,Col,Char))) :- !,
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, llo:'SyntaxError'),
  rdf_store_position(B, Line, Col, Char),
  atom_truncate(Msg1, 500, Msg2),
  rdf_store(B, llo:message, Msg2^^xsd:string).

% Timeout: read
rdf_store_message(S, P, error(timeout_error(read,_Stream),context(_Pred,_))) :- !,
  rdf_store(S, P, llo:readTimeoutError).

% Turtle: undefined prefix
rdf_store_message(S, P, error(existence_error(turtle_prefix,Prefix), stream(_Stream,Line,Col,Char))) :- !,
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, llo:'MissingTurtlePrefixDefintion'),
  rdf_store(B, llo:prefix, Prefix^^xsd:string),
  rdf_store_position(B, Line, Col, Char).

% RDF/XML: multiple definitions
rdf_store_message(S, P, rdf(redefined_id(Uri))) :- !,
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, llo:'RedefinedRdfId'),
  rdf_store(B, llo:object, Uri).

% RDF/XML: name
rdf_store_message(S, P, rdf(not_a_name(XmlName))) :- !,
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, llo:'XmlNameError'),
  rdf_store(B, llo:object, XmlName^^xsd:string).

% RDF/XML: unparsable
rdf_store_message(S, P, rdf(unparsed(Dom))) :- !,
  rdf_create_bnode(B),
  rdf_store(S, P, B),
  rdf_store(B, rdf:type, llo:'RdfXmlParserError'),
  xml_dom_to_atom(Dom, Atom1),
  atom_truncate(Atom1, 500, Atom2),
  rdf_store(B, llo:dom, Atom2^^xsd:string).



%! rdf_store_messages(+S, :Goal_0) is det.
% Run Goal, unify Result with `true`, `false` or `exception(Error)`
% and messages with a list of generated error and warning messages.
% Each message is a term `message(Term,Kind,Lines)`.

rdf_store_messages(S, Goal_0) :-
  setup_call_cleanup(
    (
      create_thread_counter(rdf_warning),
      asserta((
        user:thread_message_hook(Term,Kind,_) :-
          error_kind(Kind),
          threadsafe_format(warn, "~w~n", [Term]),
          increment_thread_counter(rdf_warning)
      ))
    ),
    (
      (   catch(Goal_0, E, true)
      ->  (   var(E)
          ->  Result = true,
              End0 = true
          ;   E = error(existence_error(open_any2,M),_)
          ->  rdf_store_metadata(S, M),
              End0 = "No stream"
          ;   Result = exception(E),
              End0 = E
          ),
          debug(rdf(debug), "[RESULT] ~w ~w", [Result,Goal_0])
      ;   msg_warning("[FAILED] ~w", [Goal_0]),
          End0 = fail
      ),
      with_output_to(string(End), write_term(End0)),
      rdf_store(S, llo:end, End^^xsd:string, meta)
    ),
    (
      delete_thread_counter(rdf_warning, N),
      rdf_store(S, llo:number_of_warnings, N^^xsd:nonNegativeInteger, meta)
    )
  ).

error_kind(warning).
error_kind(error).



%! rdf_store_metadata(+S, +M) is det.

rdf_store_metadata(S1, M) :-
  jsonld_metadata(M, Jsonld1),
  atom_string(S1, S2),
  Jsonld2 = Jsonld1.put(_{'@id': S2}),
  (debugging(rdf(debug)) -> json_write_dict(user_error, Jsonld2) ; true),
  forall(jsonld_tuple(Jsonld2, rdf(S,P,O)), (
    (debugging(rdf(debug)) -> rdf_print(S, P, O, _) ; true),
    rdf_store(S, P, O, meta)
  )).



%! rdf_show_graph(+G) is det.
%! rdf_show_graph(+G, +Opts) is det.

rdf_show_graph(G) :-
  rdf_show_graph(G, []).
rdf_show_graph(G, Opts1) :-
  rdf_graph_to_export_graph(G, ExportG, Opts1),
  file_name_extension(G, pdf, File),
  graph_viz(ExportG, File, Opts1),
  merge_options([detached(true),program('XPDF')], Opts1, Opts2),
  run_process(xpdf, [file(File)], Opts2).





% HELPERS %

%! rdf_store(+S, +P, +O) is det.
%! rdf_store(+S, +P, +O, +Alias) is det.

rdf_store(S, P, O) :-
  rdf_store(S, P, O, msg).


rdf_store(S, P, O, Alias) :-
  with_output_to(Alias, gen_ntriple(S, P, O)).



%! rdf_store_position(+S, +Line, +Col, +Char, +G) is det.

rdf_store_position(S, Line, Col, Char) :-
  rdf_create_bnode(B),
  rdf_store(S, llo:streamPosition, B),
  rdf_store(B, rdf:type, llo:'StreamPosition'),
  rdf_store(B, llo:line, Line^^xsd:nonNegativeInteger),
  rdf_store(B, llo:linePosition, Col^^xsd:nonNegativeInteger),
  rdf_store(B, llo:character, Char^^xsd:nonNegativeInteger).
