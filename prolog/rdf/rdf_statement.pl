:- module(
  rdf_statement,
  [
    rdf_tuple_terms/4,        % +Tuple, ?S, ?P, ?O
    rdf_subject_triples/2,    % +S, -Triples
    rdf_triples_datatypes/2,  % +Triples, -Ds
    rdf_triples_iris/2,       % +Triples, -Iris
    rdf_triples_iri_terms/2,  % +Triples, -Iris
    rdf_triples_predicates/2, % +Triples, -Ps
    rdf_triples_subjects/2,   % +Triples, -Ss
    rdf_triples_terms/2       % +Triples, -Ts
  ]
).

/** <module> RDF tuples

Predicates that perform simple operations on RDF triples/quadruples.

@author Wouter Beek
@version 2015/08, 2015/11-2016/01, 2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_subject_triples(r, -).





%! rdf_tuple_terms(+Tuple, -S, -P, -O) is det.

rdf_tuple_terms(rdf(S,P,O), S, P, O) :- !.
rdf_tuple_terms(rdf(S,P,O,_), S, P, O).



%! rdf_subject_triples(+S, -Triples) is det.

rdf_subject_triples(S, Triples) :-
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O), Triples).



%! rdf_triple_datatype(+Triple, -D) is semidet.

rdf_triple_datatype(rdf(_,_,O), D) :-
  rdf_literal_datatype(O, D).



%! rdf_triple_iri(+Triple, -Iri) is nondet.

rdf_triple_iri(Triple, Iri) :-
  rdf_triple_iri_term(Triple, Iri).
rdf_triple_iri(Triple, D) :-
  rdf_triple_datatype(Triple, D),
  \+ rdf_triple_iri_term(Triple, D).



%! rdf_triple_iri_term(+Triple, -Iri) is nondet.

rdf_triple_iri_term(Triple, T) :-
  rdf_triple_term(Triple, T),
  \+ rdf_is_bnode(T),
  \+ rdf_is_literal(T).



%! rdf_triple_term(+Triple, -T) is nondet.

rdf_triple_term(rdf(S,_,_), S).
rdf_triple_term(rdf(_,P,_), P).
rdf_triple_term(rdf(_,_,O), O).



%! rdf_triples_datatypes(+Triples, -Ds) is det.

rdf_triples_datatypes(Triples, Ds) :-
  aggregate_all(set(D), (member(Triple, Triples), rdf_triple_datatype(Triple, D)), Ds).



%! rdf_triples_iris(+Triples, -Iris) is det.

rdf_triples_iris(Triples, Iris) :-
  aggregate_all(
    set(Iri),
    (
      member(Triple, Triples),
      (rdf_triple_iri(Triple, Iri) ; rdf_triple_datatype(Triple, Iri))
    ),
    Iris
  ).



%! rdf_triples_iri_terms(+Triples, -Iris) is det.

rdf_triples_iri_terms(Triples, Ts) :-
  aggregate_all(set(T), (member(Triple, Triples), rdf_triple_iri(Triple, T)), Ts).



%! rdf_triples_predicates(+Triples, -Ps) is det.

rdf_triples_predicates(Triples, Ps) :-
  aggregate_all(set(P), member(rdf(_,P,_), Triples), Ps).



%! rdf_triples_subjects(+Triples, -Ss) is det.

rdf_triples_subjects(Triples, Ss) :-
  aggregate_all(set(S), member(rdf(S,_,_), Triples), Ss).



%! rdf_triples_terms(+Triples, -Ts) is det.

rdf_triples_terms(Triples, Ts) :-
  aggregate_all(set(T), (member(Triple, Triples), rdf_triple_term(Triple, T)), Ts).
