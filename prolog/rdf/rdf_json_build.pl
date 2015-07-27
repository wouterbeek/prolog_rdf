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
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_assert_json(r,r,+)).
:- rdf_meta(rdf_assert_json(r,r,+,?)).





rdf_assert_json(S, P, T):-
  rdf_assert_json(S, P, T, _).

% JSON array.
rdf_assert_json(S, P, L1, G):-
  is_list(L1), !,
  rdf_assert_list(L1, L2, G),
  rdf_assert2(S, P, L2, G).
rdf_assert_json(S, P, Json, G):-
  rdf_json_value(Json, D, Lex),
  rdf_assert2(S, P, literal(type(D,Lex)), G).

% JSON float.
rdf_json_value(X, xsd:float, Y):-
  float(X), !,
  atom_number(X, Y).
% JSON integer.
rdf_json_value(X, xsd:integer, Y):-
  integer(X), !,
  atom_number(X, Y).
% JSON string.
rdf_json_value(X, xsd:string, X):-
  string(X), !.
