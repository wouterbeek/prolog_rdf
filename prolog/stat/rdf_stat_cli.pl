:- module(
  rdf_stat_cli,
  [
    rdf_predicate_info/2,   % ?P, ?G
    rdf_predicate_types/2,  % ?P, ?G
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
:- use_module(library(list_ext)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(stat/r_ext)).
:- use_module(library(stat/rdf_stat)).
:- use_module(library(yall)).

:- rdf_meta
   rdf_predicate_info(r, r),
   rdf_predicate_types(r, r),
   rdf_predicate_values(r),
   rdf_predicate_values(r, r).





%! rdf_predicate_info(?P, ?G) is nondet.

rdf_predicate_info(P, G) :-
  rdf_predicate_values(P, G),
  rdf_predicate_types(P, G).



%! rdf_predicate_types(?P, ?G) is nondet.

rdf_predicate_types(P, G) :-
  rdf_predicate(P, G),
  rdf_aggregate_all(set(Lex), rdf(_, P, Lex^^xsd:string, G), Lexs),
  maplist(rdf_compat_datatypes0, Lexs, Dss),
  ord_intersection(Dss, Ds),
  maplist(list_split, Rows, Ds),
  rdf_print_table([head([P])|Rows]).

rdf_compat_datatypes0(Lex, Ds) :-
  aggregate_all(set(D), rdf_compat_datatype(Lex, D), Ds).



%! rdf_predicate_values(?G) is nondet.
%! rdf_predicate_values(?P, ?G) is nondet.
%
% Prints overviews of (1) how many distinct objects there are for a
% given predicate term or (2) how often each object term occurs.

rdf_predicate_values(G) :-
  rdf_graph(G),
  findall(
    N-P,
    (rdf_predicate(P, G), rdf_number_of_objects(_, P, G, N)),
    Pairs
  ),
  asc_pairs(Pairs, SortedPairs),
  maplist(pair_inv_list, SortedPairs, Rows),
  rdf_print_table([head(["Predicate","#objects"])|Rows]).


rdf_predicate_values(P, G) :-
  rdf_predicate(P, G),
  aggregate_all(set(O), rdf(_, P, O, G), Os),
  maplist({P,G}/[O,N]>>rdf_number_of_subjects(P, O, G, N), Os, Ns),
  pairs_keys_values(Pairs, Ns, Os),
  asc_pairs(Pairs, OrderedPairs),
  maplist(pair_inv_list, OrderedPairs, Rows),
  rdf_print_table([head(["Object","#occurrences"])|Rows]).
