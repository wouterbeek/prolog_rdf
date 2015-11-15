:- module(
  rdf_clean_metadata,
  [
    rdf_clean_metadata/1 % +Metadata:dict
  ]
).

/** <module> RDF Clean: Metadata

@author Wouter Beek
@version 2015/10-2015/11
*/

:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(clpfd)).
:- use_module(library(http/http_info)).





%! rdf_clean_metadata(+Metadata:dict) is det.

rdf_clean_metadata(M):-
  dcg_with_output_to(user_output, metadata(M)).

metadata(M) -->
  metadata(0, M).

metadata(I, M) -->
  {M.input_type == http_iri}, !,
  metadata_iri(I, M),
  metadata_http(I, M.http),
  metadata_archive(I, M.archive_entry),
  metadata_stream(I, M.stream),
  metadata_rdf(I, M.rdf).



metadata_archive(_, [_]) --> !, "".
metadata_archive(I, L) -->
  section(I, "Archive metadata:", (
    metadata_archive(I, L)
  )).



metadata_http(I1, M) -->
  {succ(I1, I2)},
  {succ(I2, I3)},
  {dict_pairs(M.headers, _, L)},
  section(I1, "HTTP metadata:", (
    tab_nl(I2, http_status_code(M.status_code)),
    tab_nl(I2, nvpair("Version", pl_pair(M.version))),
    tab_nl(I2, nvpair("Final IRI", iri(M.final_iri))),
    tab_nl(I2, atom("Headers:")),
    http_headers(I3, L)
  )).

http_headers(_, []) --> !, "".
http_headers(I, [N-Vs|T]) -->
  nvpairs(I, N, Vs),
  http_headers(I, T).



metadata_iri(I1, M) -->
  {succ(I1, I2)},
  section(I1, "IRI metadata:", (
    tab_nl(I2, nvpair("Base IRI", iri(M.base_iri)))
  )).



metadata_rdf(I1, M) -->
  {succ(I1, I2), succ(I2, I3)},
  section(I1, "RDF metadata:", (
    tab_nl(I2, nvpair("Serialization format", atom(M.format))),
    tab_nl(I2, nvpair("Number of statements", atom(M.statements))),
    tab_nl(I3, nvpair("Number of quadruples", atom(M.quadruples))),
    tab_nl(I3, nvpair("Number of triples", atom(M.triples)))
  )).



metadata_stream(I1, M) -->
  {succ(I1, I2)},
  {dict_pairs(M, _, L)},
  section(I1, "Stream metadata:", (
    nvpairs(I2, L)
  )).



%! nvpairs(+Indent:nonneg, +Pairs:list(pair(string)))// is det.

nvpairs(I, [N-V|T]) -->
  tab_nl(I, nvpair(N-V)), !,
  nvpairs(I, T).
nvpairs(_, []) --> "".




%! nvpairs(+Indent:nonneg, +Name:string, +Pairs:list(pair(string)))// is det.

nvpairs(I, N, [H|T]) -->
  tab_nl(I, nvpair(N-H)), !,
  nvpairs(I, N, T).
nvpairs(_, _, []) --> "".
