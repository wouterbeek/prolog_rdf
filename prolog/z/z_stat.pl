:- module(
  z_stat,
  [
    z_number_of_triples/2 % ?G, -N
  ]
).

/** <module> Z statistics

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(hdt/hdt_stat)).
:- use_module(library(rdf/rdf_stat)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   z_number_of_triples(r, -).





%! z_number_of_triples(?G, -N) is nondet.

z_number_of_triples(G, N) :-
  rdf_number_of_triples(G, N).
z_number_of_triples(G, N) :-
  hdt_number_of_triples(G, N).
