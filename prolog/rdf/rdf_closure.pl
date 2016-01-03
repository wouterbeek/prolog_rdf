:- module(
  rdf_closure,
  [
    rdf_closure/3, % ?X:term
                   % ?Predicate:iri
                   % ?Y:rdf_term
    rdf_closure0/3 % ?X:term
                   % ?Predicate:iri
                   % ?Y:rdf_term
  ]
).

/** <module> RDF Closure

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dif)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).

:- rdf_meta(rdf_closure(o,r,o)).
:- rdf_meta(rdf_closure0(o,r,o)).





rdf_closure(X, P, Z) :-
  rdf(X, P, Y),
  rdf_closure0(Y, P, Z).



rdf_closure0(X0, P, X) :-
  rdf_closure0(X0, P, X, [X0]).

rdf_closure0(X, _, X, _).
rdf_closure0(X1, P, X, Hist) :-
  rdf(X1, P, X2),
  maplist(dif(X2), Hist),
  rdf_closure0(X2, P, X, [X1|Hist]).
