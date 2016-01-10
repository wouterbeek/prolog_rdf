:- module(
  rdf_list,
  [
    rdf_nextto/3 % ?X, ?Y, ?RdfList
  ]
).
:- reexport(library(rdf11/rdf11_collections)).

/** <module> RDF lists

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(closure)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(yall)).

:- rdf_meta
   rdf_nextto(o, o, r).


%! rdf_nextto(?X, ?Y, ?RdfList) is nondet.

rdf_nextto(X, Y, L) :-
  closure([X,Y]>>rdf_directly_nextto(X, Y, L), X, Y).

rdf_directly_nextto(X, Y, L) :-
  rdf_has(L, rdf:first, X),
  rdf_has(L, rdf:rest, T),
  rdf_has(T, rdf:first, Y).
