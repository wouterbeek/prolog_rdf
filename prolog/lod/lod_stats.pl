:- module(
  lod_stats,
  [
    rdf_description_size/2, % +Resource:rdf_term
                            % -Size:nonneg
    rdf_number_of_objects/3, % ?Subject, ?Predicate, -Size
    rdf_number_of_objects/4, % ?Subject:rdf_term
                             % ?Predicate:iri
                             % ?Graph:atom
                             % -Size:nonneg
    rdf_number_of_predicates/3, % ?Subject, ?Object, -Size
    rdf_number_of_predicates/4, % ?Subject:rdf_term
                                % ?Object:rdf_term
                                % ?Graph:atom
                                % -Size:nonneg
    rdf_number_of_subjects/3, % ?Predicate, ?Object, -Size
    rdf_number_of_subjects/4, % ?Predicate:iri
                              % ?Object:rdf_term
                              % ?Graph:atom
                              % -Size:nonneg
    rdf_number_of_triples/4, % ?Subject, ?Predicate, ?Object, -Size
    rdf_number_of_triples/5 % ?Subject:rdf_term
                            % ?Predicate:iri
                            % ?Object:rdf_term
                            % ?Graph:atom
                            % -Size:nonneg
  ]
).

/** <module> LOD statistics

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(solution_sequences)).

:- rdf_meta(rdf_description_size(o,-)).
:- rdf_meta(rdf_number_of_objects(o,r,-)).
:- rdf_meta(rdf_number_of_objects(o,r,?,-)).
:- rdf_meta(rdf_number_of_predicates(o,o,-)).
:- rdf_meta(rdf_number_of_predicates(o,o,?,-)).
:- rdf_meta(rdf_number_of_subjects(r,o,-)).
:- rdf_meta(rdf_number_of_subjects(r,o,?,-)).
:- rdf_meta(rdf_number_of_triples(o,r,o,-)).
:- rdf_meta(rdf_number_of_triples(o,r,o,?,-)).





%! rdf_description_size(+Resource:rdf_term, -Size:nonneg) is det.

rdf_description_size(S, N):-
  rdf_number_of_triples(S, _, _, N).



%! rdf_number_of_objects(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_objects(S, P, N):-
  rdf_number_of_objects(S, P, _, N).


%! rdf_number_of_objects(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Graph:atom,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_objects(S, P, G, N):-
  rdf_number_of0(O, S, P, O, G, N).



%! rdf_number_of_predicates(
%!   ?Subject:rdf_term,
%!   ?Object:rdf_term,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_predicates(S, O, N):-
  rdf_number_of_predicates(S, O, _, N).


%! rdf_number_of_predicates(
%!   ?Subject:rdf_term,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_predicates(S, O, G, N):-
  rdf_number_of0(P, S, P, O, G, N).



%! rdf_number_of_subjects(
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_subjects(P, O, N):-
  rdf_number_of_subjects(P, O, _, N).


%! rdf_number_of_subjects(
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_subjects(P, O, G, N):-
  rdf_number_of0(S, S, P, O, G, N).



%! rdf_number_of_triples(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_triples(S, P, O, N):-
  rdf_number_of_triples(S, P, O, _, N).


%! rdf_number_of_triples(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_triples(S, P, O, G, N):-
  rdf_number_of0(rdf(S,P,O), S, P, O, G, N).





% HELPERS %

rdf_number_of0(Witness, S, P, O, G, N):-
  aggregate_all(count, distinct(Witness, rdf(S, P, O, G)), N).





% TESTS %

:- begin_tests(lod_stats).

test(rdf_number_of_subjects, [forall(test_case(G,N))]):-
  setup_call_cleanup(
    rdf_assert_graph(G),
    rdf_number_of_subjects(_, _, N0),
    rdf_unload_graph(G)
  ),
  writeln(N0),
  N =:= N0.

test_case(g1, 1).
test_case(g2, 2).

rdf_assert_graph(g1):-
  user:rdf_assert(rdf:s1, rdf:p1, rdf:o1, g1).
rdf_assert_graph(g2):-
  % Same subject, different predicate/object.
  user:rdf_assert(rdf:s1, rdf:p1, rdf:o1, g2),
  user:rdf_assert(rdf:s1, rdf:p2, rdf:o1, g2),
  user:rdf_assert(rdf:s1, rdf:p1, rdf:o2, g2),
  user:rdf_assert(rdf:s1, rdf:p2, rdf:o2, g2),
  % The same triple twice.
  user:rdf_assert(rdf:s2, rdf:p1, rdf:o1, g2),
  user:rdf_assert(rdf:s2, rdf:p1, rdf:o1, g2).

:- end_tests(lod_stats).
