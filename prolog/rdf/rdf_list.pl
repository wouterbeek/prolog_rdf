:- module(
  rdf_list,
  [
    rdf_nextto/3, % ?X, ?Y, ?RdfList
    rdf_last/2, % ?RdfList, ?Last
    rdf_nth0/3 % ?Index, ?RdfList, ?Element
  ]
).
:- reexport(library(rdf11/rdf11_collections)).

/** <module> RDF lists

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(closure)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(yall)).

:- rdf_meta
	rdf_nextto(o, o, r),
	rdf_last(r, o),
	rdf_nth0(?, r, o).





%! rdf_nextto(?X, ?Y, ?RdfList) is nondet.

rdf_nextto(X, Y, L) :-
  closure([X,Y]>>rdf_directly_nextto(X, Y, L), X, Y).



%! rdf_directly_nextto(?X, ?Y, ?RdfList) is nondet.

rdf_directly_nextto(X, Y, L) :-
  rdf_has(L, rdf:first, X),
  rdf_has(L, rdf:rest, T),
  rdf_has(T, rdf:first, Y).



%! rdf_last(?RdfList, ?Last) is semidet.

rdf_last(L, Last) :-
  rdf_has(L, rdf:rest, rdf:nil), !,
  rdf_has(L, rdf:first, Last).
rdf_last(L, Last) :-
  rdf_has(L, rdf:rest, T),
  rdf_last(T, Last).



%! rdf_list_nth0(?Index:nonneg, ?RdfList, ?X) is nondet.

rdf_nth0(I, L, X) :-
  rdf_nth0(0, I, L, X).

rdf_nth0(I, I, L, X) :-
  rdf_has(L, rdf:first, X).
rdf_nth0(I1, I3, L, X) :-
  rdf_has(L, rdf:rest, T),
  rdf_nth0(I1, I2, T, X),
  I3 is I2 + 1.
