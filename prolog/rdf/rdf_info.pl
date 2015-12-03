:- module(
  rdf_info,
  [
    rdf_info/0
  ]
).

/** <module> Prints executive summaries about the RDF DB

@author Wouter Beek
@version 2015/08-2015/09, 2015/12
*/

:- use_module(library(ansi_ext)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(list_ext)).
:- use_module(library(lod/lod_stats)).
:- use_module(library(rdf/rdf_print)).





%! rdf_info is det.

rdf_info:-
  % Table header.
  ansi_formatln([bold], 'Number of triples~30|Graph', []),

  % First row: all triples.
  rdf_number_of_triples(T),
  ansi_formatln([], '~D~25|All', [T]),

  % Consective rows: one for each named graph.
  findall(N-G, rdf_number_of_triples(G, N), Pairs),
  sort(1, @>=, Pairs, SortedPairs),
  list_truncate(SortedPairs, 10, TopSortedPairs),
  forall(member(N-G, TopSortedPairs), (
    dcg_with_output_to(atom(A), rdf_print_term(G)),
    ansi_formatln([], '~D~25|~a', [N,A])
  )),
  nl.
