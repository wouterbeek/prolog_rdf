:- use_module(library(llapi/llapi)).
:- use_module(library(semweb/rdf11)).

run(M) :-
  rdf_equal(skos:closeMatch, P1),
  ldf_assert_predicate(M, P1),
  rdf_equal(skos:exactMatch, P2),
  ldf_assert_predicate(M, P2).

ldf_assert_predicate(M, P) :-
  thread_create(ldf_assert_predicate0(M, P), _, []).

ldf_assert_predicate0(M, P) :-
  forall(ldf(S, P, O, G), qb(M, S, P, O, G)).
