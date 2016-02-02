:- module(
  rdf_debug,
  [
    rdf_assert_messages/4, % :Goal_0, +S, ?G, -Result
    rdf_show_graph/1,      % +G
    rdf_show_graph/2       % +G, +Opts
  ]
).

/** <module> RDF debug tools

Show RDF data structures during modeling/development.

@author Wouter Beek
@version 2015/08, 2015/12-2016/02
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(gv/gv_file)).
:- use_module(library(option)).
:- use_module(library(os/process_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_graph_viz)).
:- use_module(library(xml/xml_dom)).

:- meta_predicate
    rdf_assert_messages(0,+,?,-).

:- rdf_meta
   rdf_assert_messages(:,r,r,-),
   rdf_show_graph(r),
   rdf_show_graph(r,+).

:- predicate_options(rdf_show_graph/2, 2, [
     pass_to(rdf_graph_to_export_graph/3, 3),
     pass_to(graph_viz/3, 3),
     pass_to(run_process/3, 3)
   ]).





%! rdf_assert_message(+S, +Kind, +Term, +Lines, +G) is det.

% Archive error
rdf_assert_message(S, Kind, error(archive_error(C,_),_), _, G) :- !,
  (   C == 2
  ->  I = missingTypeKeywordInMtreeSpec
  ;   C == 25
  ->  I = invalidCentralDirectorySignature
  ),
  rdf_assert_prefixed(S, llo:Kind, llo:I, G).

% Encoding: character
rdf_assert_message(S, Kind, error(type_error(character,Char),context(_Pred,_Var)), _, G) :- !,
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert(B, rdf:type, llo:'CharacterEncodingError', G),
  rdf_assert(B, llo:object, Char^^xsd:integer, G).

% Existence llo: directory
rdf_assert_message(S, Kind, error(existence_error(directory,Dir),context(_Pred,Msg)), _, G) :- !,
  (   Msg == 'File exists'
  ->  C = 'DirectoryExistenceError'
  ),
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert_prefixed(B, rdf:type, llo:C, G),
  uri_file_name(Uri, Dir),
  rdf_assert(B, llo:object, Uri^^xsd:anyURI, G).

% Existence llo: file
rdf_assert_message(S, Kind, error(existence_error(file,File),context(_Pred,Msg)), _, G) :- !,
  (   Msg == 'Directory not empty'
  ->  C = 'DirectoryNotEmpty'
  ;   Msg == 'No such file or directory'
  ->  C = 'FileExistenceError'
  ),
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert_prefixed(B, rdf:type, llo:C, G),
  uri_file_name(Uri, File),
  rdf_assert(B, llo:object, Uri^^xsd:anyURI, G).

% Existence llo: source sink?
rdf_assert_message(S, Kind, error(existence_error(source_sink,Path),context(_Pred,Msg)), _, G) :- !,
  (   Msg == 'Is a directory'
  ->  C = 'IsADirectoryError'
  ),
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert_prefixed(B, rdf:type, llo:C, G),
  uri_file_name(Uri, Path),
  rdf_assert(B, llo:object, Uri^^xsd:anyURI, G).


% HTTP status
rdf_assert_message(S, Kind, error(http_status(Status),_), _, G) :- !,
  (   between(400, 499, Status)
  ->  rdf_assert_prefixed(S, llo:Kind, httpo-'4xxHttpError', G)
  ;   between(500, 599, Status)
  ->  rdf_assert_prefixed(S, llo:Kind, httpo-'5xxHttpError', G)
  ).

% IO llo: read
rdf_assert_message(S, Kind, error(io_error(read,_Stream),context(_Pred,Msg)), _, G) :- !,
  (   Msg == 'Connection reset by peer'
  ->  I = connectionResetByPeer
  ;   Msg == 'Inappropriate ioctl for device'
  ->  I = notATypewriter
  ;   Msg = 'Is a directory'
  ->  I = isADirectory
  ),
  rdf_assert_prefixed(S, llo:Kind, llo:I, G).

% IO llo: write
rdf_assert_message(S, Kind, error(io_error(write,_Stream),context(_Pred,Msg)), _, G) :- !,
  (   Msg == 'Encoding cannot represent character'
  ->  I = encodingError
  ),
  rdf_assert_prefixed(S, llo:Kind, llo:I, G).

% IO warning
rdf_assert_message(S, Kind, io_warning(_Stream,Msg), _, G) :- !,
  (   Msg == 'Illegal UTF-8 continuation'
  ->  I = illegalUtf8Continuation
  ;   Msg == 'Illegal UTF-8 start'
  ->  I = illegalUtf8Start
  ),
  rdf_assert_prefixed(S, llo:Kind, llo:I, G).

% Malformed URL
rdf_assert_message(S, Kind, error(domain_error(url,Url),_), _, G) :- !,
  rdf_create_bnode(B),
  rdf_assert(B, rdf:type, llo:'MalformedUrl', G),
  rdf_assert(B, llo:object, Url^^xsd:anyURI, G),
  rdf_assert_prefixed(S, llo:Kind, B, G).

% No RDF
rdf_assert_message(S, _Kind, error(no_rdf(_File)), _, G) :- !,
  rdf_assert(S, llo:serializationFormat, llo:unrecognizedFormat, G).

% Permission llo: redirect
rdf_assert_message(S, Kind, error(permission_error(Action0,Type,Object),context(_,Msg1)), _, G) :- !,
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert(B, rdf:type, llo:'PermissionError', G),
  atom_truncate(Msg1, 500, Msg2),
  rdf_assert(B, llo:message, Msg2^^xsd:string, G),

  % Action
  (   Action0 == redirect
  ->  Action = redirectAction
  ),
  rdf_assert_prefixed(B, llo:action, llo:Action, G),

  % Object
  rdf_assert(B, llo:object, Object, G),

  % Type
  (   Type == http
  ->  ObjectClass = 'HttpUri'
  ),
  rdf_assert_prefixed(Object, llo:object, llo:ObjectClass, G).

% SGML parser
rdf_assert_message(S, Kind, sgml(sgml_parser(_Parser),_File,Line,Msg1), _, G) :- !,
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert(B, rdf:type, llo:'SgmlParserError', G),
  rdf_assert(B, llo:sourceLine, Line^^xsd:nonNegativeInteger, G),
  atom_truncate(Msg1, 500, Msg2),
  rdf_assert(B, llo:message, Msg2^^xsd:string, G).

% Socket error
rdf_assert_message(S, Kind, error(socket_error(Msg),_), _, G) :- !,
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
  rdf_assert_prefixed(S, llo:Kind, llo:I, G).

% SSL llo: SSL verify
rdf_assert_message(S, Kind, error(ssl_error(ssl_verify),_), _, G) :- !,
  rdf_assert_prefixed(S, llo:Kind, llo:sslError, G).

% Syntax error
rdf_assert_message(S, Kind, error(syntax_error(Msg1),stream(_Stream,Line,Col,Char)), _, G) :- !,
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert(B, rdf:type, llo:'SyntaxError', G),
  store_position(B, Line, Col, Char, G),
  atom_truncate(Msg1, 500, Msg2),
  rdf_assert(B, llo:message, Msg2^^xsd:string, G).

% Timeout llo: read
rdf_assert_message(S, Kind, error(timeout_error(read,_Stream),context(_Pred,_)), _, G) :- !,
  rdf_assert_prefixed(S, llo:Kind, llo:readTimeoutError, G).

% Turtle: undefined prefix
rdf_assert_message(S, Kind, error(existence_error(turtle_prefix,Prefix), stream(_Stream,Line,Col,Char)), _, G) :- !,
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert(B, rdf:type, llo:'MissingTurtlePrefixDefintion', G),
  rdf_assert(B, llo:prefix, Prefix^^xsd:string, G),
  store_position(B, Line, Col, Char, G).

% RDF/XML: multiple definitions
rdf_assert_message(S, Kind, rdf(redefined_id(Uri)), _, G) :- !,
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert(B, rdf:type, llo:'RedefinedRdfId', G),
  rdf_assert(B, llo:object, Uri, G).

% RDF/XML: name
rdf_assert_message(S, Kind, rdf(not_a_name(XmlName)), _, G) :- !,
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert(B, rdf:type, llo:'XmlNameError', G),
  rdf_assert(B, llo:object, XmlName^^xsd:string, G).

% RDF/XML: unparsable
rdf_assert_message(S, Kind, rdf(unparsed(Dom)), _, G) :- !,
  rdf_create_bnode(B),
  rdf_assert_prefixed(S, llo:Kind, B, G),
  rdf_assert(B, rdf:type, llo:'RdfXmlParserError', G),
  xml_dom_to_atom(Dom, Atom1),
  atom_truncate(Atom1, 500, Atom2),
  rdf_assert(B, llo:dom, Atom2^^xsd:string, G).



%! rdf_assert_messages(:Goal_0, +S, ?G, -Result) is det.
% Run Goal, unify Result with `true`, `false` or `exception(Error)`
% and messages with a list of generated error and warning messages.
% Each message is a term `message(Term,Kind,Lines)`.

rdf_assert_messages(Goal_0, S, G, Result) :-
  (var(G) -> rdf_tmp_graph(G) ; true),
  setup_call_cleanup(
    asserta((
      user:thread_message_hook(Term,Kind,Lines) :-
        error_kind(Kind),
        rdf_assert_message(S, Kind, Term, Lines, G)
    )),
    (   catch(Goal_0, E, true)
    ->  (var(E) -> Result = true ; Result = exception(E))
    ;   Result = false
    ),
    rdf_unload_graph(G)
  ).

error_kind(warning).
error_kind(error).



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

%! rdf_assert_prefixed(+S, +P, +O, +G) is det.

rdf_assert_prefixed(S1, P1, O1, G1) :-
  maplist(rdf_global_id, [S1,P1,O1,G1], [S2,P2,O2,G2]),
  rdf_assert(S2, P2, O2, G2).



%! store_position(+S, +Line, +Col, +Char, +G) is det.

store_position(S, Line, Col, Char, G) :-
  rdf_create_bnode(B),
  rdf_assert(S, llo:streamPosition, B, G),
  rdf_assert(B, rdf:type, llo:'StreamPosition', G),
  rdf_assert(B, llo:line, Line^^xsd:nonNegativeInteger, G),
  rdf_assert(B, llo:linePosition, Col^^xsd:nonNegativeInteger, G),
  rdf_assert(B, llo:character, Char^^xsd:nonNegativeInteger, G).
