:- module(
  rdf_stat_cli,
  [
    rdf_predicate_values/0,
    rdf_predicate_values/1, %     ?G
    rdf_predicate_values/2  % ?P, ?G
  ]
).

/** <module> RDF statistics CLI

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(stat/r_ext)).
:- use_module(library(stat/rdf_stat)).
:- use_module(library(yall)).





rdf_predicate_values :-
  forall(rdf_graph(G), rdf_predicate_values(G)).


rdf_predicate_values(G) :-
  findall(
    N-P,
    (rdf_predicate(P, G), rdf_number_of_objects(_, P, G, N)),
    Pairs
  ),
  asc_pairs(Pairs, SortedPairs),
  maplist(pair_inv_list, SortedPairs, Rows),
  print_table([head(["Predicate","#objects"])|Rows]).


rdf_predicate_values(P, G) :-
  rdf_predicate(P, G),
  aggregate_all(set(O), rdf(_, P, O, G), Os),
  maplist({P,G}/[O,N]>>rdf_number_of_subjects(P, O, G, N), Os, Ns),
  pairs_keys_values(Pairs, Ns, Os),
  asc_pairs(Pairs, OrderedPairs),
  maplist(pair_inv_list, OrderedPairs, Rows),
  rdf_print_table([head(["Object","#occurrences"])|Rows]).
