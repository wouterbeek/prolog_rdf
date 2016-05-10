% Tests for `rdf/rdf_stats`.

:- use_module(library(plunit)).
:- use_module(library(rdf/rdf_stats)).
:- use_module(library(rdf/rdf_test), []).

:- begin_tests(rdf_stats).

test(rdf_number_of_subjects, [forall(test_case(G,N))]) :-
  setup_call_cleanup(
    rdf_assert_graph(G),
    rdf_number_of_subjects(_, _, G, N0),
    rdf_unload_graph(G)
  ),
  writeln(N0),
  N =:= N0.

test_case(ex:g1, 1).
test_case(ex:g2, 2).

rdf_assert_graph(g1) :-
  rdf_assert(rdf:s1, rdf:p1, rdf:o1, ex:g1).
rdf_assert_graph(g2) :-
  % Same subject, different predicate/object.
  rdf_assert(rdf:s1, rdf:p1, rdf:o1, ex:g2),
  rdf_assert(rdf:s1, rdf:p2, rdf:o1, ex:g2),
  rdf_assert(rdf:s1, rdf:p1, rdf:o2, ex:g2),
  rdf_assert(rdf:s1, rdf:p2, rdf:o2, ex:g2),
  % The same triple twice.
  rdf_assert(rdf:s2, rdf:p1, rdf:o1, ex:g2),
  rdf_assert(rdf:s2, rdf:p1, rdf:o1, ex:g2).

:- end_tests(rdf_stats).
