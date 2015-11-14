:- module(
  rdf_statement,
  [
    rdf_statement_terms/4, % +Statement:compound
                           % ?Subject:rdf_term
                           % ?Predocate:iri
                           % ?Object:rdf_term
    rdf_triples_to_iris/2, % +Triples:list(compound)
                           % -Iris:ordset(atom)
    rdf_triples_to_terms/2 % +Triples:list(compound)
                           % -Terms:ordset(rdf_term)
  ]
).

/** <module> RDF statements

Predicates that perform simple operations on RDF triples/quadruples.

@author Wouter Beek
@version 2015/08, 2015/11
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).





%! rdf_statement_terms(
%!   +Statement:compound,
%!   -Subject:rdf_term,
%!   -Predicate:iri,
%!   -Object:rdf_term
%! ) is det.

rdf_statement_terms(rdf(S,P,O), S, P, O):- !.
rdf_statement_terms(rdf(S,P,O,_), S, P, O).



%! rdf_triples_to_iris(+Triples:list(compound), -Iris:ordset(atom)) is det.

rdf_triples_to_iris(Ts, Iris):-
  aggregate_all(set(Iri), (member(T, Ts), rdf_triple_to_iri(T, Iri)), Iris).

%! rdf_triples_to_terms(
%!   +Triples:list(compound),
%!   -Terms:ordset(compound)
%! ) is det.

rdf_triples_to_terms(Ts, Xs):-
  aggregate_all(set(X), (member(T, Ts), rdf_triple_to_term(T, X)), Xs).


%! rdf_triple_to_iri(+Triple:compound, -Iri:iri) is nondet.

rdf_triple_to_iri(T, X):-
  rdf_triple_to_term(T, X),
  \+ rdf_is_bnode(X),
  \+ rdf_is_literal(X).


%! rdf_triple_to_term(+Triple:compound, -Term:rdf_term) is nondet.

rdf_triple_to_term(rdf(S,_,_), S).
rdf_triple_to_term(rdf(_,P,_), P).
rdf_triple_to_term(rdf(_,_,O), O).
