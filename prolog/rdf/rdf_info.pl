:- module(
  rdf_info,
  [
    rdf_info/0
  ]
).

/** <module> Prints executive summaries about the RDF DB

@author Wouter Beek
@version 2015/08-2015/09
*/

:- use_module(library(ansi_ext)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).





%! rdf_info is det.

rdf_info:-
  % Table header.
  ansi_formatln([bold], 'Number of triples~30|Graph', []),

  % First row: all triples.
  rdf_statistics(triples(T)),
  ansi_formatln([], '~D~25|All', [T]),

  % Consective rows: one for each named graph.
  findall(N-G, rdf_statistics(triples_by_graph(G,N)), Pairs),
  sort(1, @>=, Pairs, Sorted),
  (   length(Prefix, 10),
      append(Prefix, _, Sorted)
  ->  true
  ;   Prefix = Sorted
  ),
  forall(member(N-G, Prefix), (
    dcg_with_output_to(atom(G0), rdf_print_term(G)),
    ansi_formatln([], '~D~25|~a', [N,G0])
  )),

  nl.
