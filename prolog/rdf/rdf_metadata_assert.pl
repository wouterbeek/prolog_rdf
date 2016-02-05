:- module(
  rdf_metadata_assert,
  [
    rdf_metadata_assert/2 % +X, +Metadata
  ]
).

/** <module> RDF Clean: Metadata

@author Wouter Beek
@version 2015/10-2015/11, 2016/01
*/

:- use_module(library(apply)).
:- use_module(library(http/http_info)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf11/rdf11), [
     op(110, xfx, @),
     op(650, xfx, ^^)
   ]).





%! rdf_metadata_assert(+X, +M) is det.

rdf_metadata_assert(X, M) :-
  (M.source_type == http_iri -> assert_http_iri(X, M) ; true),
  (get_dict(archive_entry, M, MEntry) -> assert_archive(X, MEntry) ; true),
  (get_dict(stream, M, MStream) -> assert_stream(X, MStream) ; true),
  (get_dict(rdf, M, MRdf) -> assert_rdf(X, MRdf) ; true).

assert_archive(X, M) :-
  gtrace,
  writeln(X-M).

assert_http(X, M) :-
  assert_http_status_code(X, M.status_code),
  assert_http_version(X, M.version),
  rdf_assert(X, llo:final_iri, M.final_iri^^xsd:anyURI),
  dict_pairs(M.headers, _, L),
  maplist(assert_http_headers(X), L).

assert_http_headers(X, Key-Values) :-
  maplist(assert_http_header(X, Key), Values).

assert_http_header(X, Key, Value) :-
  assert_dict_as_rdf(Value, B),
  rdf_global_id(llo:Key, P),
  rdf_assert(X, P, B).

assert_http_iri(X, M) :-
  rdf_assert(X, llo:base_iri, M.'llo:base-iri'^^xsd:anyURI),
  assert_http(X, M.http).

assert_http_status_code(X, Code) :-
  http_status_label(Code, Label),
  rdf_assert(X, llo:status_code, Code^^xsd:nonNegativeInteger),
  rdf_assert(X, llo:status_label, Label@'en-US').

assert_http_version(X, Major-Minor) :-
  rdf_create_bnode(B),
  rdf_assert(X, llo:version, B),
  rdf_assert(B, llo:major, Major^^xsd:nonNegativeInteger),
  rdf_assert(B, llo:minor, Minor^^xsd:nonNegativeInteger).

assert_rdf(X, M) :-
  rdf_assert(X, llo:serialization_format, M.format^^xsd:string),
  rdf_assert(X, llo:processed_statements, M.processed.statements^^xsd:nonNegativeInteger),
  rdf_assert(X, llo:processed_quadruples, M.processed.quadruples^^xsd:nonNegativeInteger),
  rdf_assert(X, llo:processed_triples, M.processed.triples^^xsd:nonNegativeInteger),
  rdf_assert(X, llo:statements, M.unique_statements^^xsd:nonNegativeInteger),
  rdf_assert(X, llo:duplicates, M.duplicate_statements^^xsd:nonNegativeInteger).

assert_stream(X, M) :-
  rdf_assert(X, llo:byte_count, M.position.byte_count^^xsd:nonNegativeInteger),
  rdf_assert(X, llo:char_count, M.position.char_count^^xsd:nonNegativeInteger),
  rdf_assert(X, llo:line_count, M.position.line_count^^xsd:nonNegativeInteger).





% HELPERS %

assert_dict_as_rdf(D, B) :-
  rdf_create_bnode(B),
  dict_pairs(D, _, L),
  maplist(assert_pair_as_rdf(B), L).

assert_pair_as_rdf(X, K-V) :-
  gtrace,
  rdf_global_id(llo:K, P),
  (   is_dict(V) -> assert_dict_as_rdf(V, O), rdf_assert(X, P, O)
  ;   is_list(V) -> rdf_assert_list(V, O), rdf_assert(X, P, O, default)
  ;                 rdf_assert(X, P, V^^xsd:string)
  ).

/* @tbd Why?
assert_pair_as_rdf(X, K-V) :-
  rdf_global_id(llo:K, P),
  (   is_dict(V)
  ->  assert_dict_as_rdf(V, O)
  ;   is_list(V)
  ->  rdf_assert_list(V, O)
  ;   % @tbd Why not?
      O = V^^xsd:string
  ),
  rdf_assert(X, P, O).
*/
