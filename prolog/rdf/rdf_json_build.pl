:- module(
  rdf_json_build,
  [
    rdf_assert_json/3, % +Subject, +Predicate, +JsonTerm
    rdf_assert_json/4 % +Subject:or([bnode,iri])
                      % +Predicate:iri
                      % +JsonTerm
                      % ?Graph:atom
  ]
).

/** <module> RDF JSON build

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_list)).

:- rdf_meta(rdf_assert_json(r,r,+)).
:- rdf_meta(rdf_assert_json(r,r,+,?)).



rdf_assert_json(S, P, T):-
  rdf_assert_json(S, P, T, _).

% JSON array.
rdf_assert_json(S, P, L1, G):-
  is_list(L1), !,
  rdf_assert_list(L1, L2, G),
  rdf_assert2(S, P, L2, G).
% JSON value.
rdf_assert_json(S, P, Json, G):-
  rdf_json_value(Json, D, Lex),
  rdf_assert2(S, P, literal(type(D,Lex)), G).

% JSON number.
rdf_json_value(X, D, Y):-
  (   % JSON float.
      float(X)
  ->  rdf_equal(xsd:float, D),
      atom_number(Y, X)
  ;   % JSON integer.
      integer(X)
  ->  rdf_equal(xsd:integer, D),
      atom_number(Y, X)
  ), !.
% JSON string.
rdf_json_value(X, D, X):-
  string(X), !,
  rdf_equal(xsd:string, D).
