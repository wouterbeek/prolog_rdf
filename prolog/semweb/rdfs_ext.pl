:- module(
  rdfs_ext,
  [
    rdfs_range/4 % +M, ?P, ?C, ?G
  ]
).
:- reexport(library(semweb/rdf_ext)).

/** <module> RDFS extensions

@author Wouter Beek
@version 2017/08
*/

:- rdf_meta
   rdfs_range(+, r, r, r).





%! rdfs_range(+M, ?P, ?C, ?G) is nondet.

rdfs_range(M, P, C, G) :-
  rdf(M, P, rdfs:range, C, G).
rdfs_range(M, P, C, G) :-
  rdf(M, P, rdfs:subPropertyOf, Q, G),
  (P == Q -> print_message(warning, direct_cycle(P)) ; true),
  rdfs_range(M, Q, C, G).
