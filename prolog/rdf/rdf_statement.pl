:- module(
  rdf_statement,
  [
    rdf_graph_triples/2, % ?Graph;rdf_graph
                         % -Triples:ordset(rdf_triple)
    rdf_statement_terms/4, % +Statement:rdf_statement
                           % ?Subject:rdf_term
                           % ?Predocate:iri
                           % ?Object:rdf_term
    rdf_triples_iris/2, % +Triples:list(compound)
                        % -Iris:ordset(atom)
    rdf_triples_subjects/2, % +Triples:list(compound)
                            % -Subjects:ordset(rdf_term)
    rdf_triples_terms/2 % +Triples:list(compound)
                        % -Terms:ordset(rdf_term)
  ]
).

/** <module> RDF statements

Predicates that perform simple operations on RDF triples/quadruples.

@author Wouter Beek
@version 2015/08, 2015/11-2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).





%! rdf_graph_triples(?Graph;rdf_graph, -Triples:ordset(rdf_triple)) is det.

rdf_graph_triples(G, Ts):-
  rdf_expect_graph(G),
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O, G), Ts).



%! rdf_statement_terms(
%!   +Statement:rdf_statement,
%!   -Subject:rdf_term,
%!   -Predicate:iri,
%!   -Object:rdf_term
%! ) is det.

rdf_statement_terms(rdf(S,P,O), S, P, O):- !.
rdf_statement_terms(rdf(S,P,O,_), S, P, O).



%! rdf_triple_iri(+Triple:rdf_triple, -Iri:iri) is nondet.

rdf_triple_iri(T, X):-
  rdf_triple_term(T, X),
  \+ rdf_is_bnode(X),
  \+ rdf_is_literal(X).



%! rdf_triple_term(+Triple:rdf_triple, -Term:rdf_term) is nondet.

rdf_triple_term(rdf(S,_,_), S).
rdf_triple_term(rdf(_,P,_), P).
rdf_triple_term(rdf(_,_,O), O).



%! rdf_triples_iris(+Triples:list(rdf_triple), -Iris:ordset(iri)) is det.

rdf_triples_iris(Ts, Iris):-
  aggregate_all(set(Iri), (member(T, Ts), rdf_triple_iri(T, Iri)), Iris).



%! rdf_triples_subjects(
%!   +Triples:list(rdf_triple),
%!   -Subjects:ordset(rdf_term)
%! ) is det.

rdf_triples_subjects(Ts, Ss):-
  aggregate_all(set(S), member(rdf(S,_,_), Ts), Ss).



%! rdf_triples_terms(
%!   +Triples:list(rdf_triple),
%!   -Terms:ordset(rdf_term)
%! ) is det.

rdf_triples_terms(Ts1, Ts2):-
  aggregate_all(set(T2), (member(T1, Ts1), rdf_triple_term(T1, T2)), Ts2).
