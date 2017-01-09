:- module(
  rdf_error,
  [
    rdf_store_warning/3 % +M, +Doc, +Term
  ]
).

/** <module> RDF error

@author Wouter Beek
@version 2016/03-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(default)).
:- use_module(library(pl_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).





%! rdf_store_warning(+M, +Doc, +E) is det.

% Archive error
rdf_store_warning(M, Doc, error(archive_error(Code,Msg),_)) :-
  (   Msg = 'Invalid central directory signature'
  ->  Name = invalid_central_directory_signature
  ;   Msg = 'Missing type keyword in mtree specification'
  ->  Name = missing_type_keyword_in_mtree_specification
  ;   Msg = 'Unrecognized archive format'
  ->  Name = unrecognized_archive_format
  ;   % E.g. “Truncated input file (needed 186262016 bytes, only 0
      % available)”
      Code =:= 25
  ->  Name = truncated_input_file
  ;   % E.g. “Truncated input file (needed 186262016 bytes, only 0
      % available)”
      Code =:= 1001
  ->  Name = truncated_input_file
  ), !,
  rdf_global_id(nsdef:Name, O),
  qb(M, Doc, nsdef:archive_error, O).
% Archive cannot parse line
rdf_store_warning(M, Doc, error(archive_error(_,Msg),_)) :-
  atom_phrase(archive_cant_parse_line(Line), Msg), !,
  qb(M, Doc, nsdef:archive_cannot_parse_line, Line^^xsd:nonNegativeInteger).
% Archive has no content
rdf_store_warning(M, Doc, error(no_content(_),_)) :-!,
  qb(M, Doc, nsdef:archive_error, nsdef:no_content).
% Cookie
rdf_store_warning(M, Doc, error(domain_error(set_cookie,Msg),_)) :- !,
  qb(M, Doc, nsdef:set_cookie, Msg^^xsd:string).
% Encoding: character
rdf_store_warning(M, Doc, error(type_error(character,Char),context(_,_))) :- !,
  qb(M, Doc, nsdef:character_encoding_error, Char^^xsd:integer).
% Existence: directory
rdf_store_warning(M, Doc, error(existence_error(directory,Dir),context(_,'File exists'))) :- !,
  uri_file_name(Uri, Dir),
  qb(M, Doc, nsdef:directory_existence_error, Uri^^xsd:anyURI).
% Existence: file
rdf_store_warning(M, Doc, error(existence_error(file,File),context(_,Msg))) :-
  (   Msg == 'Directory not empty'
  ->  Name = directory_not_empty
  ;   Msg == 'No such file or directory'
  ->  Name = file_existence_error
  ), !,
  rdf_global_id(nsdef:Name, P),
  uri_file_name(Uri, File),
  qb(M, Doc, P, Uri^^xsd:anyURI).
% Existence: source sink?
rdf_store_warning(M, Doc, error(existence_error(source_sink,Path),context(_,'Is a directory'))) :- !,
  uri_file_name(Uri, Path),
  qb(M, Doc, nsdef:is_a_directory_error, Uri^^xsd:anyURI).
% HTTP reply is empty
rdf_store_warning(M, Doc, error(existence_error(http_reply,_),_)) :- !,
  qb(M, Doc, nsdef:http_error, nsdef:empty_http_reply).
% I/O: read
rdf_store_warning(M, Doc, error(io_error(read,_),context(_,Msg))) :-
  (   Msg == 'Connection reset by peer'
  ->  Name = connection_reset_by_peer
  ;   Msg == 'Inappropriate ioctl for device'
  ->  Name = not_a_typewriter
  ;   Msg = 'Is a directory'
  ->  Name = is_a_directory
  ), !,
  rdf_global_id(nsdef:Name, O),
  qb(M, Doc, nsdef:io_read_error, O).
% I/O: write
rdf_store_warning(M, Doc, error(io_error(write,_),context(_,'Encoding cannot represent character'))) :- !,
  qb(M, Doc, nsdef:io_write_error, nsdef:encoding_error).
% I/O warning
rdf_store_warning(M, Doc, io_warning(_,Msg)) :-
  (   Msg == 'Illegal UTF-8 continuation'
  ->  Name = illegal_utf8_continuation
  ;   Msg == 'Illegal UTF-8 start'
  ->  Name = illegal_utf8_start
  ),
  rdf_global_id(nsdef:Name, O),
  qb(M, Doc, nsdef:io_warning, O).
% IRI
rdf_store_warning(M, Doc, E) :-
  (   E = error(existence_error(source_sink,Name),_)
  ->  true
  ;   E = error(representation_error(max_path_length),context(system:is_absolute_file_name/1, _))
  ->  Name = Doc
  ;   E = error(permission_error(open,source_sink,Name),context(system:open/4,'Permission denied'))
  ->  true
  ), !,
  qb(M, Doc, nsdef:not_an_iri, Name^^xsd:string).
% ???
rdf_store_warning(M, Doc, error(type_error(http_iri,Name),_)) :- !,
  qb(M, Doc, nsdef:non_https_iri, Name^^xsd:anyURI).
% Literal: illegal lexical form.
rdf_store_warning(M, Doc, error(type_error(D,Lex),_)) :- !,
  q_tbox_iri(D, P),
  qb(M, Doc, P, Lex^^xsd:string).
% Literal: out-of-bounds value.
rdf_store_warning(M, Doc, error(domain_error(D,Lex),_)) :- !,
  q_tbox_iri(D, P),
  qb(M, Doc, P, Lex^^xsd:string).
% Literal: non-canonical lexical form.
rdf_store_warning(M, Doc, non_canonical_lexical_form(D,Lex)) :- !,
  q_tbox_iri(D, P),
  qb(M, Doc, P, Lex^^xsd:string).
% Malformed URL
rdf_store_warning(M, Doc, error(domain_error(url,Url),_)) :- !,
  qb(M, Doc, nsdef:malformed_url, Url^^xsd:anyURI).
% Cannot guess whether this is RDF
rdf_store_warning(M, Doc, error(domain_error(cannot_guess_rdf_media_type,Uri),_)) :- !,
  qb(M, Doc, nsdef:cannot_guess_rdf_media_type, Uri).
% No RDF
rdf_store_warning(M, Doc, error(domain_error(rdf_media_type,MT),_)) :- !,
  qb(M, Doc, nsdef:no_rdf_media_type, MT^^xsd:string).
% Permission: redirect
rdf_store_warning(M, Doc, error(permission_error(redirect,http,Object),context(_,Msg1))) :- !,
  atom_ellipsis(Msg1, 500, Msg2),
  format(string(String), "[~a] ~a", [Object,Msg2]),
  qb(M, Doc, nsdef:http_redirect_permission_error, String^^xsd:string).
% SGML parser
rdf_store_warning(M, Doc, sgml(sgml_parser(_),_,Line,Msg1)) :- !,
  atom_ellipsis(Msg1, 500, Msg2),
  format(string(String), "[~w] ~a", [Line,Msg2]),
  qb(M, Doc, nsdef:sgml_parser_error, String^^xsd:string).
% Socket error
rdf_store_warning(M, Doc, error(socket_error(Msg),_)) :-
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
  rdf_global_id(nsdef:Name, O),
  qb(M, Doc, nsdef:socket_error, O).
% SSL: error
rdf_store_warning(M, Doc, error(ssl_error(Error0,_Lib0,_Func0,_Reason0),_)) :- !,
  atom_number(Error0, Error),
  %atom_phrase(ssl_lib(Lib), Lib0),
  %atom_phrase(ssl_func(Func), Func0),
  %atom_phrase(ssl_reason(Reason), Reason0),
  qb(M, Doc, nsdef:ssl_error, Error^^xsd:nonNegativeInteger).
% SSL: verify
rdf_store_warning(M, Doc, error(ssl_error(ssl_verify),_)) :- !,
  qb(M, Doc, nsdef:ssl_error, nsdef:ssl_verify).
% Syntax error
rdf_store_warning(M, Doc, error(syntax_error(Term),stream(_,Line,Col,Char))) :- !,
  with_output_to(atom(A1), write_term(Term)),
  atom_ellipsis(A1, 500, A2),
  maplist(defval(unknown), [Line,Col,Char]),
  format(string(String), "[~w:~w:~w] ~a", [Line,Col,Char,A2]),
  qb(M, Doc, nsdef:syntax_error, String^^xsd:string).
% Timeout: read
rdf_store_warning(M, Doc, error(timeout_error(read,_),context(_,_))) :- !,
  qb(M, Doc, nsdef:timeout_error, nsdef:read).
% Turtle: undefined prefix
rdf_store_warning(M, Doc, error(existence_error(turtle_prefix,Prefix), stream(_,Line,Col,Char))) :- !,
  format(string(String), "[~w:~w:~w] ~a", [Line,Col,Char,Prefix]),
  qb(M, Doc, nsdef:missing_turtle_prefix_defintion, String^^xsd:string).
% RDF/XML: multiple definitions
rdf_store_warning(M, Doc, rdf(redefined_id(Uri))) :- !,
  qb(M, Doc, nsdef:redefined_rdf_id, Uri^^xsd:anyURI).
% RDF/XML: name
rdf_store_warning(M, Doc, rdf(not_a_name(XmlName))) :- !,
  qb(M, Doc, nsdef:xml_name_error, XmlName^^xsd:string).
% RDF/XML: unexpected tag name.
rdf_store_warning(M, Doc, rdf(unexpected(TagName,_SgmlParser))) :- !,
  format(atom(Term), "~w", [TagName]),
  qb(M, Doc, nsdef:rdf_xml_unexpected, Term^^xsd:string).
% RDF/XML: cannot parse DOM.
rdf_store_warning(M, Doc, rdf(unparsed(Dom))) :- !,
  rdf11:in_xml_literal(xml, Dom, A1),
  atom_ellipsis(A1, 500, A2),
  qb(M, Doc, nsdef:rdf_xml_parser_error, A2^^xsd:string).
% XML: DOM error
rdf_store_warning(M, Doc, error(type_error(xml_dom,A1),_)) :- !,
  atom_ellipsis(A1, 500, A2),
  qb(M, Doc, nsdef:no_xml_dom, A2^^xsd:string).
rdf_store_warning(M, Doc, Term) :-
  debug(true, "~w,~w,~w", [M,Doc,Term]).

archive_cant_parse_line(Line) --> "Can't parse line ", integer(Line).

ssl_lib(N) --> "lib(", integer(N), ")".

ssl_func(N) --> "func(", integer(N), ")".

ssl_reason(N) --> "reason(", integer(N), ")".
