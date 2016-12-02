% Tests for `rdf/rdf_stat`.

:- use_module(library(plunit)).
:- use_module(library(rdf/rdf_stat)).

:- use_module('../rdf_test_data').

:- begin_tests(rdf_stat).

test(q_number_of_subjects, [forall(test_case0(G,NumSs))]) :-
  M = trp,
  qb_graph0(M, G),
  q_number_of_subjects(M, G, NumSs0),
  q_unload(M, G)
  writeln(NumSs0),
  NumSs =:= NumSs0.

test_case0(ex:g1, 1).
test_case0(ex:g2, 2).

qb_graph0(M, g1) :-
  qb(M, rdf:s1, rdf:p1, rdf:o1, ex:g1).
qb_graph0(M, g2) :-
  % Same subject, different predicate/object.
  qb(M, rdf:s1, rdf:p1, rdf:o1, ex:g2),
  qb(M, rdf:s1, rdf:p2, rdf:o1, ex:g2),
  qb(M, rdf:s1, rdf:p1, rdf:o2, ex:g2),
  qb(M, rdf:s1, rdf:p2, rdf:o2, ex:g2),
  % The same triple twice.
  qb(M, rdf:s2, rdf:p1, rdf:o1, ex:g2),
  qb(M, rdf:s2, rdf:p1, rdf:o1, ex:g2).

:- end_tests(rdf_stat).
