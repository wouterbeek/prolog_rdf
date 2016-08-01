:- module(
  rdf_stat,
  [
    rdf_number_of_predicates/1, % -NumPs
    rdf_number_of_predicates/2, % ?G, -NumPs
    rdf_number_of_triples/1,    % -NumTriples
    rdf_number_of_triples/2     % ?G, -NumTriples
  ]
).

/** <module> Z statistics

@author Wouter Beek
@version 2016/06, 2016/08
*/

:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_number_of_predicates(r, -),
   rdf_number_of_triples(r, -).





%! rdf_number_of_predicates(-NumPs) is nondet.
%! rdf_number_of_predicates(?G, -NumPs) is nondet.

rdf_number_of_predicates(NumPs) :-
  rdf_statistics(predicates(NumPs)).


rdf_number_of_predicates(G, NumPs) :-
  aggregate_all(count, rdf_predicate(_, G), NumPs).



%! rdf_number_of_triples(-NumTriples) is nondet.
%! rdf_number_of_triples(?G, -NumTriples) is nondet.

rdf_number_of_triples(NumTriples) :-
  rdf_statistics(triples(NumTriples)).


rdf_number_of_triples(G, NumTriples) :-
  rdf_graph_property(G, triples(NumTriples)).
