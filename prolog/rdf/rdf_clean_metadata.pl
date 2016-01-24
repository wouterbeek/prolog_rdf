:- module(
  rdf_clean_metadata,
  [
    rdf_clean_metadata/1 % +M
  ]
).

/** <module> RDF Clean: Metadata

@author Wouter Beek
@version 2015/10-2015/11, 2016/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_cardinal)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(clpfd)).
:- use_module(library(http/http_info)).





%! http_header(+Indent, +Header:pair)// is det.

http_header(I, _-Vs) -->
  {is_list(Vs)}, !,
  http_header_values(I, Vs).
http_header(I, N-V) -->
  http_header(I, N-[V]).



http_header_status(_-Vs, Comp) :-
  (Vs = [V|_] ; Vs = V),
  http_header_status_comparator(V.status, Comp).



http_header_status_comparator(unrecognized, <).
http_header_status_comparator(invalid,      =).
http_header_status_comparator(valid,        >).



http_header_values(I, [H|T]) --> !,
  tab(I), {string_codes(H.raw, Cs)}, string(Cs), nl,
  http_header_values(I, T).
http_header_values(_, []) --> "".



%! http_headers(+Indent, +Headers:list(pair))// is det.

http_headers(I, L) -->
  {partition(http_header_status, L, L1, L2, L3)},
  http_headers(I, L1, L2, L3).



%! http_headers(
%!   +Indent,
%!   +Unrecognized:list(pair),
%!   +Invalid:list(pair),
%!   +Valid:list(pair)
%! )// is det.

http_headers(I1, L1, L2, L3) -->
  {I2 is I1 + 1},
  section(I1, "Unrecognized:", *(http_header(I2), L1)),
  section(I1, "Invalid:", *(http_header(I2), L2)),
  section(I1, "Valid:", *(http_header(I2), L3)).



%! rdf_clean_metadata(+M) is det.

rdf_clean_metadata(M) :-
  dcg_with_output_to(user_output, rdf_clean_metadata(M)).

rdf_clean_metadata(M) -->
  rdf_clean_metadata(0, M).

rdf_clean_metadata(I, M) -->
  {M.source_type == http_iri}, !,
  metadata_http_iri(I, M).
rdf_clean_metadata(I, M) -->
  metadata_generic(I, M).



metadata_archive(_, [_]) --> !, "".
metadata_archive(I, L) -->
  section(I, "Archive metadata:", metadata_archive(I, L)).



metadata_generic(I, M) -->
  (   {get_dict(archive_entry, M, MArchiveEntry)}
  ->  metadata_archive(I, MArchiveEntry)
  ;   ""
  ),
  ({get_dict(stream, M, MStream)} -> metadata_stream(I, MStream) ; ""),
  ({get_dict(rdf, M, MRdf)} -> metadata_rdf(I, MRdf) ; "").



metadata_http(I1, M) -->
  {
    I2 is I1 + 1,
    I3 is I2 + 1,
    dict_pairs(M.headers, _, L)
  },
  section(I1, "HTTP metadata:", (
    tab_nl(I2, http_status_code(M.status_code)),
    tab_nl(I2, nvpair("Version", pl_pair(M.version))),
    tab_nl(I2, nvpair("Final IRI", iri(M.final_iri))),
    section(I2, "Headers:", http_headers(I3, L))
  )).



metadata_http_iri(I, M) -->
  metadata_iri(I, M),
  metadata_http(I, M.http),
  metadata_generic(I, M).



metadata_iri(I1, M) -->
  {succ(I1, I2)},
  section(I1, "IRI metadata:", tab_nl(I2, nvpair("Base IRI", iri(M.base_iri)))).



metadata_rdf(I1, M) -->
  {
    succ(I1, I2),
    succ(I2, I3)
  },
  section(I1, "RDF metadata:", (
    tab_nl(I2, nvpair("Serialization format", atom(M.format))),
    tab_nl(I2, nvpair("Number of statements", thousands_integer(M.statements))),
    tab_nl(I3, nvpair("Number of quadruples", thousands_integer(M.quadruples))),
    tab_nl(I3, nvpair("Number of triples", thousands_integer(M.triples)))
  )).



metadata_stream(I1, M) -->
  {
    succ(I1, I2),
    dict_pairs(M, _, L)
  },
  section(I1, "Stream metadata:", nvpairs(I2, L)).



%! nvpairs(+Indent:nonneg, +Pairs:list(pair(string)))// is det.

nvpairs(I, [N-V|T]) -->
  tab_nl(I, nvpair(N-V)), !,
  nvpairs(I, T).
nvpairs(_, []) --> "".
