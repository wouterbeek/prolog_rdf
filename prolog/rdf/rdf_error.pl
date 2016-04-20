:- module(
  rdf_error,
  [
    rdf_store/4,        % +Out, +S, +P, +O
    rdf_store_list/3,   % +Out, +L, -RdfL
    rdf_store_warning/3 % +Out, +Doc, +E
  ]
).

/** <module> RDF error

@author Wouter Beek
@version 2016/03-2016/04
*/

:- use_module(library(atom_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- rdf_meta
   rdf_store(r, r, r, o).





%! rdf_store(+Out, +S, +P, +O) is det.

rdf_store(Out, S, P, O) :-
  is_stream(Out), !,
  with_output_to(Out, gen_ntriple(S, P, O)).
rdf_store(G, S, P, O) :-
  rdf_assert(S, P, O, G).



%! rdf_store_list(+Out, +L, -RdfL) is det.

rdf_store_list(Out, L, B) :-
  rdf_create_bnode(B),
  rdf_store_list0(Out, B, L).


rdf_store_list0(_, _, []) :- !.
rdf_store_list0(Out, B1, [H|T]) :-
  rdf_store(Out, B1, rdf:first, H),
  (T == [] -> rdf_equal(rdf:nil, B2) ; rdf_create_bnode(B2)),
  rdf_store(Out, B1, rdf:rest, B2),
  rdf_store_list0(Out, B2, T).



%! rdf_store_warning(+Out, +Doc, +E) is det.

% Archive error
rdf_store_warning(Out, Doc, error(archive_error(Code,_),_)) :-
  (   Code == 2
  ->  Name = missing_type_keyword_in_mtree_spec
  ;   Code == 25
  ->  Name = invalid_central_directory_signature
  ), !,
  rdf_global_id(llo:Name, O),
  rdf_store(Out, Doc, llo:arhive_error, O).
% Encoding: character
rdf_store_warning(Out, Doc, error(type_error(character,Char),context(_,_))) :- !,
  rdf_store(Out, Doc, llo:character_encoding_error, Char^^xsd:integer).
% Existence: directory
rdf_store_warning(Out, Doc, error(existence_error(directory,Dir),context(_,'File exists'))) :- !,
  uri_file_name(Uri, Dir),
  rdf_store(Out, Doc, llo:directory_existence_error, Uri^^xsd:anyURI).
% Existence: file
rdf_store_warning(Out, Doc, error(existence_error(file,File),context(_,Msg))) :-
  (   Msg == 'Directory not empty'
  ->  Name = directory_not_empty
  ;   Msg == 'No such file or directory'
  ->  Name = file_existence_error
  ), !,
  rdf_global_id(llo:Name, P),
  uri_file_name(Uri, File),
  rdf_store(Out, Doc, P, Uri^^xsd:anyURI).
% Existence: source sink?
rdf_store_warning(Out, Doc, error(existence_error(source_sink,Path),context(_,'Is a directory'))) :- !,
  uri_file_name(Uri, Path),
  rdf_store(Out, Doc, llo:is_a_directory_error, Uri^^xsd:anyURI).
% HTTP status
rdf_store_warning(Out, Doc, error(http_status(Status),_)) :-
  (between(400, 499, Status) ; between(500, 599, Status)), !,
  rdf_store(Out, Doc, llo:http_error, Status^^xsd:positiveInteger).
% IO: read
rdf_store_warning(Out, Doc, error(io_error(read,_),context(_,Msg))) :-
  (   Msg == 'Connection reset by peer'
  ->  Name = connection_reset_by_peer
  ;   Msg == 'Inappropriate ioctl for device'
  ->  Name = not_a_typewriter
  ;   Msg = 'Is a directory'
  ->  Name = is_a_directory
  ), !,
  rdf_global_id(llo:Name, O),
  rdf_store(Out, Doc, llo:io_read_error, O).
% IO: write
rdf_store_warning(Out, Doc, error(io_error(write,_),context(_,'Encoding cannot represent character'))) :- !,
  rdf_store(Out, Doc, llo:io_write_error, llo:encoding_error).
% IO warning
rdf_store_warning(Out, Doc, io_warning(_,Msg)) :-
  (   Msg == 'Illegal UTF-8 continuation'
  ->  Name = illegal_utf8_continuation
  ;   Msg == 'Illegal UTF-8 start'
  ->  Name = illegal_utf8_start
  ),
  rdf_global_id(llo:Name, O),
  rdf_store(Out, Doc, llo:io_warning, O).
% IRI
rdf_store_warning(Out, Doc, E) :-
  (   E = error(existence_error(source_sink,Name),_)
  ->  true
  ;   E = error(representation_error(max_path_length),context(system:is_absolute_file_name/1, _))
  ->  Name = Doc
  ;   E = error(permission_error(open,source_sink,Name),context(system:open/4,'Permission denied'))
  ->  true
  ), !,
  rdf_store(Out, Doc, llo:not_an_iri, Name^^xsd:string).
rdf_store_warning(Out, Doc, error(type_error(http_iri,Name),_)) :- !,
  rdf_store(Out, Doc, llo:non_https_iri, Name^^xsd:anyURI).
% Literal: non-canonical lexical form.
rdf_store_warning(Out, Doc, non_canonical_lexical_form(D1,Lex)) :- !,
  abbr_iri(D1, D2),
  atom_concat(noncanonical_, D2, Name),
  rdf_global_id(llo:Name, P),
  rdf_store(Out, Doc, P, Lex^^xsd:string).
% Malformed URL
rdf_store_warning(Out, Doc, error(domain_error(url,Url),_)) :- !,
  rdf_store(Out, Doc, llo:malformed_url, Url^^xsd:anyURI).
% No RDF
rdf_store_warning(Out, Doc, error(no_rdf(_))) :- !,
  rdf_store(Out, Doc, llo:rdf_serialization_format, llo:unrecognized_format).
% Permission: redirect
rdf_store_warning(Out, Doc, error(permission_error(redirect,http,Object),context(_,Msg1))) :- !,
  atom_truncate(Msg1, 500, Msg2),
  format(string(String), "[~a] ~a", [Object,Msg2]),
  rdf_store(Out, Doc, llo:http_redirect_permission_error, String^^xsd:string).
% SGML parser
rdf_store_warning(Out, Doc, sgml(sgml_parser(_),_,Line,Msg1)) :- !,
  atom_truncate(Msg1, 500, Msg2),
  format(string(String), "[~w] ~a", [Line,Msg2]),
  rdf_store(Out, Doc, llo:sgml_parser_error, String^^xsd:string).
% Socket error
rdf_store_warning(Out, Doc, error(socket_error(Msg),_)) :-
  (   Msg == 'Connection timed out'
  ->  Name = connection_timed_out
  ;   Msg == 'Connection refused'
  ->  Name = connection_refused
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
  rdf_global_id(llo:Name, O),
  rdf_store(Out, Doc, llo:socket_error, O).
% SSL: verify
rdf_store_warning(Out, Doc, error(ssl_error(ssl_verify),_)) :- !,
  rdf_store(Out, Doc, llo:ssl_error, llo:ssl_verify).
% Syntax error
rdf_store_warning(Out, Doc, error(syntax_error(Msg1),stream(_,Line,Col,Char))) :- !,
  atom_truncate(Msg1, 500, Msg2),
  format(string(String), "[~w:~w:~w] ~a", [Line,Col,Char,Msg2]),
  rdf_store(Out, Doc, llo:syntax_error, String^^xsd:string).
% Timeout: read
rdf_store_warning(Out, Doc, error(timeout_error(read,_),context(_,_))) :- !,
  rdf_store(Out, Doc, llo:timeout_error, llo:read).
% Turtle: undefined prefix
rdf_store_warning(Out, Doc, error(existence_error(turtle_prefix,Prefix), stream(_,Line,Col,Char))) :- !,
  format(string(String), "[~w:~w:~w] ~a", [Line,Col,Char,Prefix]),
  rdf_store(Out, Doc, llo:missing_turtle_prefix_defintion, String^^xsd:string).
% RDF/XML: multiple definitions
rdf_store_warning(Out, Doc, rdf(redefined_id(Uri))) :- !,
  rdf_store(Out, Doc, llo:redefined_rdf_id, Uri^^xsd:anyURI).
% RDF/XML: name
rdf_store_warning(Out, Doc, rdf(not_a_name(XmlName))) :- !,
  rdf_store(Out, Doc, llo:xml_name_error, XmlName^^xsd:string).
% RDF/XML: cannot parse
rdf_store_warning(Out, Doc, rdf(unparsed(Dom))) :- !,
  rdf11:in_xml_literal(xml, Dom, A1),
  atom_truncate(A1, 500, A2),
  rdf_store(Out, Doc, llo:rdf_xml_parser_error, A2^^xsd:string).
% Unhandled error term.
rdf_store_warning(_, _, Term) :-
  gtrace,
  msg_warning("~w~n", [Term]).
