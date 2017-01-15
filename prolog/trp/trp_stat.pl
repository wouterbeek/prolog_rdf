:- module(
  trp_stat,
  [
    trp_number_of_predicates/1, % -NumPs
    trp_number_of_predicates/2, % ?G, -NumPs
    trp_number_of_triples/1,    % -NumTriples
    trp_number_of_triples/2     % ?G, -NumTriples
  ]
).

/** <module> In-memory triples: statistics

@author Wouter Beek
@version 2016/06, 2016/08, 2017/01
*/

:- use_module(library(trp/trp_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   trp_number_of_predicates(r, -),
   trp_number_of_triples(r, -).





%! trp_number_of_predicates(-NumPs) is nondet.
%! trp_number_of_predicates(?G, -NumPs) is nondet.

trp_number_of_predicates(NumPs) :-
  trp_statistics(predicates(NumPs)).


trp_number_of_predicates(G, NumPs) :-
  aggregate_all(count, trp_predicate(_, G), NumPs).



%! trp_number_of_triples(-NumTriples) is nondet.
%! trp_number_of_triples(?G, -NumTriples) is nondet.

trp_number_of_triples(NumTriples) :-
  trp_statistics(triples(NumTriples)).


trp_number_of_triples(G, NumTriples) :-
  trp_graph_property(G, triples(NumTriples)).
