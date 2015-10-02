:- module(
  rdf_clean_metadata,
  [
    rdf_clean_metadata/1 % +Metadata:dict
  ]
).

/** <module> RDF Clean: Metadata

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(clpfd)).
:- use_module(library(http/http_deb)).





%! rdf_clean_metadata(+Metadata:dict) is det.

rdf_clean_metadata(M):-
  dcg_with_output_to(user_output, metadata(M)).

metadata(M) -->
  metadata(0, M).

metadata(I, M) -->
  {M.input_type == iri}, !,
  metadata_iri(I, M),
  metadata_http(I, M.http),
  metadata_compression(I, M.compression),
  metadata_rdf(I, M).



metadata_compression(_, [_]) --> !, "".
metadata_compression(I, [H|T]) -->
  indent(I), atom(H),
  metadata_compression(I, T).



metadata_http(I1, M) -->
  "HTTP metadata:", nl,
  {I2 #= I1 + 1},
  indent(I2), http_status_code(M.status_code), nl,
  indent(I2), nvpair("Version", nvpair(M.version)), nl,
  indent(I2), nvpair("Final IRI", iri(M.final_iri)), nl,
  indent(I2), "Headers:", nl,
  {I3 #= I2 + 1},
  {dict_pairs(M.headers, _, L)},
  http_headers(I3, L).

http_headers(_, []) --> !, "".
http_headers(I, [N-Vs|T]) -->
  http_headers_same_name(I, N, Vs),
  http_headers(I, T).

http_headers_same_name(_, _, []) --> !, "".
http_headers_same_name(I, N, [H|T]) -->
  indent(I), nvpair(N-H), nl,
  http_headers_same_name(I, N, T).



metadata_iri(I1, M) -->
  "IRI metadata:", nl,
  {I2 #= I1 + 1},
  indent(I2), nvpair("Base IRI", iri(M.base_iri)), nl.
  


metadata_rdf(I1, M) -->
  "RDF metadata:", nl,
  {I2 #= I1 + 1},
  indent(I2), nvpair("Format", atom(M.rdf_format)), nl.
