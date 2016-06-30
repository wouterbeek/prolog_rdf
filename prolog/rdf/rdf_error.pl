:- module(
  rdf_error,
  [
    rdf_store_warning/3 % +Sink, +Doc, +E
  ]
).

/** <module> RDF error

@author Wouter Beek
@version 2016/03-2016/05
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(default)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_store)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- qb_alias(deref, 'http://lodlaundromat.org/deref/').





%! rdf_store_warning(+Out, +Doc, +E) is det.

% Archive error
rdf_store_warning(Out, Doc, error(archive_error(_,Msg),_)) :-
  (   Msg = 'Invalid central directory signature'
  ->  Name = invalid_central_directory_signature
  ;   Msg = 'Missing type keyword in mtree specification'
  ->  Name = missing_type_keyword_in_mtree_specification
  ;   Msg = 'Unrecognized archive format'
  ->  Name = unrecognized_archive_format
  ), !,
  rdf_global_id(deref:Name, O),
  rdf_store(Out, Doc, deref:archive_error, O).
% Archive cannot parse line
rdf_store_warning(Out, Doc, error(archive_error(_,Msg),_)) :-
  atom_phrase(archive_cant_parse_line(Line), Msg), !,
  rdf_store(Out, Doc, deref:archive_cannot_parse_line, Line^^xsd:nonNegativeInteger).
% Archive has no content
rdf_store_warning(Out, Doc, error(no_content(_),_)) :-!,
  rdf_store(Out, Doc, deref:archive_error, deref:no_content).
% Cookie
rdf_store_warning(Out, Doc, error(domain_error(set_cookie,Msg),_)) :- !,
  rdf_store(Out, Doc, deref:set_cookie, Msg^^xsd:string).
% Encoding: character
rdf_store_warning(Out, Doc, error(type_error(character,Char),context(_,_))) :- !,
  rdf_store(Out, Doc, deref:character_encoding_error, Char^^xsd:integer).
% Existence: directory
rdf_store_warning(Out, Doc, error(existence_error(directory,Dir),context(_,'File exists'))) :- !,
  uri_file_name(Uri, Dir),
  rdf_store(Out, Doc, deref:directory_existence_error, Uri^^xsd:anyURI).
% Existence: file
rdf_store_warning(Out, Doc, error(existence_error(file,File),context(_,Msg))) :-
  (   Msg == 'Directory not empty'
  ->  Name = directory_not_empty
  ;   Msg == 'No such file or directory'
  ->  Name = file_existence_error
  ), !,
  rdf_global_id(deref:Name, P),
  uri_file_name(Uri, File),
  rdf_store(Out, Doc, P, Uri^^xsd:anyURI).
% Existence: source sink?
rdf_store_warning(Out, Doc, error(existence_error(source_sink,Path),context(_,'Is a directory'))) :- !,
  uri_file_name(Uri, Path),
  rdf_store(Out, Doc, deref:is_a_directory_error, Uri^^xsd:anyURI).
% HTTP reply is empty
rdf_store_warning(Out, Doc, error(existence_error(http_reply,_),_)) :- !,
  rdf_store(Out, Doc, deref:http_error, deref:empty_http_reply).
% IO: read
rdf_store_warning(Out, Doc, error(io_error(read,_),context(_,Msg))) :-
  (   Msg == 'Connection reset by peer'
  ->  Name = connection_reset_by_peer
  ;   Msg == 'Inappropriate ioctl for device'
  ->  Name = not_a_typewriter
  ;   Msg = 'Is a directory'
  ->  Name = is_a_directory
  ), !,
  rdf_global_id(deref:Name, O),
  rdf_store(Out, Doc, deref:io_read_error, O).
% IO: write
rdf_store_warning(Out, Doc, error(io_error(write,_),context(_,'Encoding cannot represent character'))) :- !,
  rdf_store(Out, Doc, deref:io_write_error, deref:encoding_error).
% IO warning
rdf_store_warning(Out, Doc, io_warning(_,Msg)) :-
  (   Msg == 'Illegal UTF-8 continuation'
  ->  Name = illegal_utf8_continuation
  ;   Msg == 'Illegal UTF-8 start'
  ->  Name = illegal_utf8_start
  ),
  rdf_global_id(deref:Name, O),
  rdf_store(Out, Doc, deref:io_warning, O).
% IRI
rdf_store_warning(Out, Doc, E) :-
  (   E = error(existence_error(source_sink,Name),_)
  ->  true
  ;   E = error(representation_error(max_path_length),context(system:is_absolute_file_name/1, _))
  ->  Name = Doc
  ;   E = error(permission_error(open,source_sink,Name),context(system:open/4,'Permission denied'))
  ->  true
  ), !,
  rdf_store(Out, Doc, deref:not_an_iri, Name^^xsd:string).
rdf_store_warning(Out, Doc, error(type_error(http_iri,Name),_)) :- !,
  rdf_store(Out, Doc, deref:non_https_iri, Name^^xsd:anyURI).
% Literal: illegal lexical form.
rdf_store_warning(Out, Doc, error(type_error(D1,Lex),_)) :- !,
  abbr_iri(D1, D2),
  atom_concat(illegal_, D2, Name),
  rdf_global_id(deref:Name, P),
  rdf_store(Out, Doc, P, Lex^^xsd:string).
% Literal: out-of-bounds value.
rdf_store_warning(Out, Doc, error(domain_error(D1,Lex),_)) :- !,
  abbr_iri(D1, D2),
  atom_concat(outofbounds_, D2, Name),
  rdf_global_id(deref:Name, P),
  rdf_store(Out, Doc, P, Lex^^xsd:string).
% Literal: non-canonical lexical form.
rdf_store_warning(Out, Doc, non_canonical_lexical_form(D1,Lex)) :- !,
  abbr_iri(D1, D2),
  atom_concat(noncanonical_, D2, Name),
  rdf_global_id(deref:Name, P),
  rdf_store(Out, Doc, P, Lex^^xsd:string).
% Malformed URL
rdf_store_warning(Out, Doc, error(domain_error(url,Url),_)) :- !,
  rdf_store(Out, Doc, deref:malformed_url, Url^^xsd:anyURI).
% No RDF
rdf_store_warning(Out, Doc, error(domain_error(rdf_format,Format),_)) :- !,
  rdf_store(Out, Doc, deref:no_rdf_serialization_format, Format^^xsd:string).
% Permission: redirect
rdf_store_warning(Out, Doc, error(permission_error(redirect,http,Object),context(_,Msg1))) :- !,
  atom_truncate(Msg1, 500, Msg2),
  format(string(String), "[~a] ~a", [Object,Msg2]),
  rdf_store(Out, Doc, deref:http_redirect_permission_error, String^^xsd:string).
% SGML parser
rdf_store_warning(Out, Doc, sgml(sgml_parser(_),_,Line,Msg1)) :- !,
  atom_truncate(Msg1, 500, Msg2),
  format(string(String), "[~w] ~a", [Line,Msg2]),
  rdf_store(Out, Doc, deref:sgml_parser_error, String^^xsd:string).
% Socket error
rdf_store_warning(Out, Doc, error(socket_error(Msg),_)) :-
  (   Msg == 'Connection timed out'
  ->  Name = connection_timed_out
  ;   Msg == 'Connection refused'
  ->  Name = connection_refused
  ;   Msg == 'Network is unreachable'
  ->  Name = unreachable_network
  ;   Msg == 'No Data'
  ->  Name = no_data
  ;   Msg == 'No Recovery'
  ->  Name = no_recovery
  ;   Msg == 'No route to host'
  ->  Name = no_route_to_host
  ;   Msg == 'Host not found'
  ->  Name = host_not_found
  ;   Msg == 'Try Again'
  ->  Name = try_again
  ;   Msg = 'Unknown error 0' % @tbd ???
  ->  Name = unknown_error_0  % @tbd ???
  ), !,
  rdf_global_id(deref:Name, O),
  rdf_store(Out, Doc, deref:socket_error, O).
% SSL: error
rdf_store_warning(Out, Doc, error(ssl_error(Error0,_Lib0,_Func0,_Reason0),_)) :- !,
  atom_number(Error0, Error),
  %atom_phrase(ssl_lib(Lib), Lib0),
  %atom_phrase(ssl_func(Func), Func0),
  %atom_phrase(ssl_reason(Reason), Reason0),
  rdf_store(Out, Doc, deref:ssl_error, Error^^xsd:nonNegativeInteger).
% SSL: verify
rdf_store_warning(Out, Doc, error(ssl_error(ssl_verify),_)) :- !,
  rdf_store(Out, Doc, deref:ssl_error, deref:ssl_verify).
% Syntax error
rdf_store_warning(Out, Doc, error(syntax_error(Msg1),stream(_,Line,Col,Char))) :- !,
  atom_truncate(Msg1, 500, Msg2),
  maplist(defval(unknown), [Line,Col,Char]),
  format(string(String), "[~w:~w:~w] ~a", [Line,Col,Char,Msg2]),
  rdf_store(Out, Doc, deref:syntax_error, String^^xsd:string).
% Timeout: read
rdf_store_warning(Out, Doc, error(timeout_error(read,_),context(_,_))) :- !,
  rdf_store(Out, Doc, deref:timeout_error, deref:read).
% Turtle: undefined prefix
rdf_store_warning(Out, Doc, error(existence_error(turtle_prefix,Prefix), stream(_,Line,Col,Char))) :- !,
  format(string(String), "[~w:~w:~w] ~a", [Line,Col,Char,Prefix]),
  rdf_store(Out, Doc, deref:missing_turtle_prefix_defintion, String^^xsd:string).
% RDF/XML: multiple definitions
rdf_store_warning(Out, Doc, rdf(redefined_id(Uri))) :- !,
  rdf_store(Out, Doc, deref:redefined_rdf_id, Uri^^xsd:anyURI).
% RDF/XML: name
rdf_store_warning(Out, Doc, rdf(not_a_name(XmlName))) :- !,
  rdf_store(Out, Doc, deref:xml_name_error, XmlName^^xsd:string).
% RDF/XML: cannot parse
rdf_store_warning(Out, Doc, rdf(unparsed(Dom))) :- !,
  rdf11:in_xml_literal(xml, Dom, A1),
  atom_truncate(A1, 500, A2),
  rdf_store(Out, Doc, deref:rdf_xml_parser_error, A2^^xsd:string).
% XML: DOM error
rdf_store_warning(Out, Doc, error(type_error(xml_dom,A1),_)) :- !,
  atom_truncate(A1, 500, A2),
  rdf_store(Out, Doc, deref:no_xml_dom, A2^^xsd:string).
% Unhandled error term.
rdf_store_warning(_, _, Term) :-
  forall(between(1, 1000000, _), format(user_output, "~w~n", [Term])),
  msg_warning("~w~n", [Term]).



archive_cant_parse_line(Line) --> "Can't parse line ", integer(Line).

ssl_lib(N) --> "lib(", integer(N), ")".

ssl_func(N) --> "func(", integer(N), ")".

ssl_reason(N) --> "reason(", integer(N), ")".
