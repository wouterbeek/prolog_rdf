:- module(
  hdt_stat,
  [
    hdt_number_of_triples/2 % ?G, -N
  ]
).

/** <module> HDT statistics

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(hdt/hdt_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   hdt_number_of_triples(r, -).





%! hdt_number_of_triples(?G, -N) is nondet.

hdt_number_of_triples(G, N) :-
  hdt_header(_, '<http://rdfs.org/ns/void#triples>', N, G).
