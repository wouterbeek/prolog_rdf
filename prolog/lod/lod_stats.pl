:- module(
  lod_stats,
  [
    rdf_description_size/2, % +Resource:rdf_term
                            % -Count:nonneg
    rdf_number_of_object_terms/1, % -Count
    rdf_number_of_object_terms/2, % ?Graph, -Count
    rdf_number_of_object_terms/3, % ?Subject, ?Predicate, -Count
    rdf_number_of_object_terms/4, % ?Subject:rdf_term
                                  % ?Predicate:iri
                                  % ?Graph:atom
                                  % -Count:nonneg
    rdf_number_of_predicate_terms/1, % -Count
    rdf_number_of_predicate_terms/2, % ?Graph, -Count
    rdf_number_of_predicate_terms/3, % ?Subject, ?Object, -Count
    rdf_number_of_predicate_terms/4, % ?Subject:rdf_term
                                     % ?Object:rdf_term
                                     % ?Graph:atom
                                     % -Count:nonneg
    rdf_number_of_subject_terms/1, % -Count
    rdf_number_of_subject_terms/2, % ?Graph, -Count
    rdf_number_of_subject_terms/3, % ?Predicate, ?Object, -Count
    rdf_number_of_subject_terms/4, % ?Predicate:iri
                                   % ?Object:rdf_term
                                   % ?Graph:atom
                                   % -Count:nonneg
    rdf_number_of_triples/1, % -Count
    rdf_number_of_triples/2, % ?Graph, -Count
    rdf_number_of_triples/4, % ?Subject, ?Predicate, ?Object, -Count
    rdf_number_of_triples/5 % ?Subject:rdf_term
                            % ?Predicate:iri
                            % ?Object:rdf_term
                            % ?Graph:atom
                            % -Count:nonneg
  ]
).

/** <module> LOD statistics

@author Wouter Beek
@version 2015/08, 2015/10, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(plunit)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db), [rdf_statistics/1]).
:- use_module(library(solution_sequences)).

:- rdf_meta(rdf_description_size(o,-)).
:- rdf_meta(rdf_number_of_object_terms(o,r,-)).
:- rdf_meta(rdf_number_of_object_terms(o,r,?,-)).
:- rdf_meta(rdf_number_of_predicate_terms(o,o,-)).
:- rdf_meta(rdf_number_of_predicate_terms(o,o,?,-)).
:- rdf_meta(rdf_number_of_subject_terms(r,o,-)).
:- rdf_meta(rdf_number_of_subject_terms(r,o,?,-)).
:- rdf_meta(rdf_number_of_triples(o,r,o,-)).
:- rdf_meta(rdf_number_of_triples(o,r,o,?,-)).





%! rdf_description_size(+Resource:rdf_term, -Count:nonneg) is det.

rdf_description_size(S, N):-
  rdf_number_of_triples(S, _, _, N).



%! rdf_number_of_object_terms(-Count:nonneg) is det.

rdf_number_of_object_terms(N):-
  aggregate_all(count, rdf_object(_), N).


%! rdf_number_of_object_terms(+Graph:atom, -Count:nonneg) is det.
%! rdf_number_of_object_terms(-Graph:atom, -Count:nonneg) is nondet.
% @throws exitence_error

rdf_number_of_object_terms(G, N):-
  rdf_expect_graph(G),
  rdf_number_of_object_terms(_, _, G, N).


%! rdf_number_of_object_terms(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_object_terms(S, P, N):-
  rdf_number_of_object_terms(S, P, _, N).


%! rdf_number_of_object_terms(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Graph:atom,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_object_terms(S, P, G, N):-
  rdf_number_of_terms0(O, S, P, O, G, N).



%! rdf_number_of_predicate_terms(-Count:nonneg) is det.

rdf_number_of_predicate_terms(N):-
  aggregate_all(count, rdf_predicate(_), N).


%! rdf_number_of_predicate_terms(+Graph:atom, -Count:nonneg) is det.
%! rdf_number_of_predicate_terms(-Graph:atom, -Count:nonneg) is nondet.

rdf_number_of_predicate_terms(G, N):-
  rdf_expect_graph(G),
  rdf_number_of_predicate_terms(_, _, G, N).


%! rdf_number_of_predicate_terms(
%!   ?Subject:rdf_term,
%!   ?Object:rdf_term,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_predicate_terms(S, O, N):-
  rdf_number_of_predicate_terms(S, O, _, N).


%! rdf_number_of_predicate_terms(
%!   ?Subject:rdf_term,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_predicate_terms(S, O, G, N):-
  rdf_number_of_terms0(P, S, P, O, G, N).



%! rdf_number_of_subject_terms(-Count:nonneg) is det.

rdf_number_of_subject_terms(N):-
  aggregate_all(count, rdf_subject(_), N).


%! rdf_number_of_subject_terms(+Graph:atom, -Count:nonneg) is det.
%! rdf_number_of_subject_terms(-Graph:atom, -Count:nonneg) is nondet.

rdf_number_of_subject_terms(G, N):-
  rdf_expect_graph(G),
  rdf_number_of_subject_terms(_, _, G, N).


%! rdf_number_of_subject_terms(
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_subject_terms(P, O, N):-
  rdf_number_of_subject_terms(P, O, _, N).


%! rdf_number_of_subject_terms(
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_subject_terms(P, O, G, N):-
  rdf_number_of_terms0(S, S, P, O, G, N).



%! rdf_number_of_triples(-Count:nonneg) is det.

rdf_number_of_triples(N):-
  rdf_statistics(triples(N)).


%! rdf_number_of_triples(+Graph:atom, -Count:nonneg) is det.
%! rdf_number_of_triples(-Graph:atom, -Count:nonneg) is nondet.
% @throws existence_error

rdf_number_of_triples(G, N):-
  rdf_statistics(triples_by_graph(G,N)).


%! rdf_number_of_triples(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_triples(S, P, O, N):-
  rdf_number_of_triples(S, P, O, _, N).


%! rdf_number_of_triples(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_triples(S, P, O, G, N):-
  rdf_number_of_terms0(rdf(S,P,O), S, P, O, G, N).





% HELPERS %

%! rdf_expect_graph(@Term) is nondet.
% If Term is uninstantiated it is non-deterministically
% instantiated to RDF graphs.
% If Term is instantiated and does not denote an existing RDF graph
% this results in an exception.
%
% @throws existence_error

rdf_expect_graph(G):-
  var(G), !,
  % NONDET.
  rdf_graph(G).
rdf_expect_graph(G):-
  rdf_is_graph(G), !.
rdf_expect_graph(G):-
  existence_error(rdf_graph, G).



rdf_number_of_terms0(Witness, S, P, O, G, N):-
  aggregate_all(count, distinct(Witness, rdf(S, P, O, G)), N).





% TESTS %

:- begin_tests(lod_stats).

test(rdf_number_of_subject_terms, [forall(test_case(G,N))]):-
  setup_call_cleanup(
    rdf_assert_graph(G),
    rdf_number_of_subject_terms(_, _, G, N0),
    rdf_unload_graph(G)
  ),
  writeln(N0),
  N =:= N0.

test_case(ex:g1, 1).
test_case(ex:g2, 2).

rdf_assert_graph(g1):-
  rdf_assert(rdf:s1, rdf:p1, rdf:o1, ex:g1).
rdf_assert_graph(g2):-
  % Same subject, different predicate/object.
  rdf_assert(rdf:s1, rdf:p1, rdf:o1, ex:g2),
  rdf_assert(rdf:s1, rdf:p2, rdf:o1, ex:g2),
  rdf_assert(rdf:s1, rdf:p1, rdf:o2, ex:g2),
  rdf_assert(rdf:s1, rdf:p2, rdf:o2, ex:g2),
  % The same triple twice.
  rdf_assert(rdf:s2, rdf:p1, rdf:o1, ex:g2),
  rdf_assert(rdf:s2, rdf:p1, rdf:o1, ex:g2).

:- end_tests(lod_stats).
