:- module(
  rdf_statement,
  [
    rdf_graph_triples/2,      % ?G, -Trips
    rdf_statement_terms/4,    % +Stmt, ?S, ?P, ?O
    rdf_subject_triples/2,    % +S, -Trips
    rdf_triples_iris/2,       % +Trips, -Iris
    rdf_triples_predicates/2, % +Trips, -Ps
    rdf_triples_subjects/2,   % +Trips, -Ss
    rdf_triples_terms/2       % +Trips, -Ts
  ]
).

/** <module> RDF statements

Predicates that perform simple operations on RDF triples/quadruples.

@author Wouter Beek
@version 2015/08, 2015/11-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf11/rdf11)).

:- rdf_meta
   rdf_graph_triples(r, -),
   rdf_subject_triples(r, -).





% ! rdf_graph_triples(?G, -Trips) is det.

rdf_graph_triples(G, Trips) :-
  rdf_expect_graph(G),
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O, G), Trips).



%! rdf_statement_terms(+Stmt, -S, -P, -O) is det.

rdf_statement_terms(rdf(S,P,O), S, P, O) :- !.
rdf_statement_terms(rdf(S,P,O,_), S, P, O).



%! rdf_subject_triples(+S, -Trips) is det.

rdf_subject_triples(S, Trips) :-
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O), Trips).



%! rdf_triple_iri(+Trip, -Iri) is nondet.

rdf_triple_iri(Trip, T) :-
  rdf_triple_term(Trip, T),
  \+ rdf_is_bnode(T),
  \+ rdf_is_literal(T).



%! rdf_triple_term(+Trip, -T) is nondet.

rdf_triple_term(rdf(S,_,_), S).
rdf_triple_term(rdf(_,P,_), P).
rdf_triple_term(rdf(_,_,O), O).



%! rdf_triples_iris(+Trips, -Iris) is det.

rdf_triples_iris(Trips, Ts) :-
  aggregate_all(set(T), (member(Trip, Trips), rdf_triple_iri(Trip, T)), Ts).



%! rdf_triples_predicates(+Trips, -Ps) is det.

rdf_triples_predicates(Trips, Ps) :-
  aggregate_all(set(P), member(rdf(_,P,_), Trips), Ps).



%! rdf_triples_subjects(+Trips, -Ss) is det.

rdf_triples_subjects(Trips, Ss) :-
  aggregate_all(set(S), member(rdf(S,_,_), Trips), Ss).



%! rdf_triples_terms(+Trips, -Ts) is det.

rdf_triples_terms(Trips, Ts) :-
  aggregate_all(set(T), (member(Trip, Trips), rdf_triple_term(Trip, T)), Ts).
