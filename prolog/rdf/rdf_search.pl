:- module(
  rdf_search,
  [
    rdf_search/2 % +Pattern, -S
  ]
).

/** <module> RDF search

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_search(+, t).





%! rdf_search(+Pattern, -Quad) is nondet.

rdf_search(Pattern, rdf(S,P,Str^^D,G)) :-
  {like(Str, Pattern)},
  rdf(S, P, Str^^D, G).
