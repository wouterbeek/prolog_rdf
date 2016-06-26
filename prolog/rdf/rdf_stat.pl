:- module(
  rdf_stat,
  [
    rdf_number_of_triples/1, % -N
    rdf_number_of_triples/2  % ?G, -N
  ]
).

/** <module> Z statistics

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_number_of_triples(r, -).





%! rdf_number_of_triples(-N) is nondet.
%! rdf_number_of_triples(?G, -N) is nondet.

rdf_number_of_triples(N) :-
  rdf_statistics(triples(N)).


rdf_number_of_triples(G, N) :-
  rdf_graph_property(G, triples(N)).
