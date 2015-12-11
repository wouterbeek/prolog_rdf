:- module(
  lod_stats,
  [
    rdf_description_size/2, % +Resource:rdf_term
                            % -Count:nonneg
    rdf_graph_property/2, % ?Graph:rdf_graph
                          % ?Property:compound
    rdf_number_of_bnodes/1, % -Count
    rdf_number_of_bnodes/2, % ?Graph:rdf_graph
                            % -Count:nonneg
    rdf_number_of_datatype_iris/1, % -Count
    rdf_number_of_datatype_iris/2, % ?Graph:rdf_graph
                                   % -Count:nonneg
    rdf_number_of_instances/2, % +Class:rdf_term
                               % -Count:nonneg
    rdf_number_of_objects/1, % -Count
    rdf_number_of_objects/2, % ?Graph, -Count
    rdf_number_of_objects/3, % ?Subject, ?Predicate, -Count
    rdf_number_of_objects/4, % ?Subject:rdf_term
                             % ?Predicate:iri
                             % ?Graph:rdf_graph
                             % -Count:nonneg
    rdf_number_of_predicates/1, % -Count
    rdf_number_of_predicates/2, % ?Graph, -Count
    rdf_number_of_predicates/3, % ?Subject, ?Object, -Count
    rdf_number_of_predicates/4, % ?Subject:rdf_term
                                % ?Object:rdf_term
                                % ?Graph:rdf_graph
                                % -Count:nonneg
    rdf_number_of_subjects/1, % -Count
    rdf_number_of_subjects/2, % ?Graph, -Count
    rdf_number_of_subjects/3, % ?Predicate, ?Object, -Count
    rdf_number_of_subjects/4, % ?Predicate:iri
                              % ?Object:rdf_term
                              % ?Graph:rdf_graph
                              % -Count:nonneg
    rdf_number_of_triples/1, % -Count
    rdf_number_of_triples/2, % ?Graph, -Count
    rdf_number_of_triples/4, % ?Subject, ?Predicate, ?Object, -Count
    rdf_number_of_triples/5 % ?Subject:rdf_term
                            % ?Predicate:iri
                            % ?Object:rdf_term
                            % ?Graph:rdf_graph
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
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db), [
     rdf_graph_property/2 as rdf_graph_property0,
     rdf_statistics/1 as rdf_statistics0
   ]).
:- use_module(library(semweb/rdfs), [rdfs_individual_of/2]).
:- use_module(library(solution_sequences)).

:- rdf_meta(rdf_description_size(o,-)).
:- rdf_meta(rdf_graph_property(r,?)).
:- rdf_meta(rdf_number_of_bnodes(r,-)).
:- rdf_meta(rdf_number_of_datatype_iris(r,-)).
:- rdf_meta(rdf_number_of_objects(o,r,-)).
:- rdf_meta(rdf_number_of_objects(o,r,?,-)).
:- rdf_meta(rdf_number_of_predicates(o,o,-)).
:- rdf_meta(rdf_number_of_predicates(o,o,?,-)).
:- rdf_meta(rdf_number_of_subjects(r,o,-)).
:- rdf_meta(rdf_number_of_subjects(r,o,?,-)).
:- rdf_meta(rdf_number_of_triples(o,r,o,-)).
:- rdf_meta(rdf_number_of_triples(o,r,o,?,-)).





%! rdf_description_size(+Resource:rdf_term, -Count:nonneg) is det.

rdf_description_size(S, N):-
  rdf_number_of_triples(S, _, _, N).



%! rdf_graph_property(?Graph:rdf_graph, ?Property:compound) is nondet.

rdf_graph_property(G, P):-
  rdf_graph_property0(G, P).



%! rdf_number_of_bnodes(-Count:nonneg) is det.

rdf_number_of_bnodes(N):-
  aggregate_all(count, rdf_bnode(_), N).


%! rdf_number_of_bnodes(+Graph:rdf_graph, -Count:nonneg) is det.
%! rdf_number_of_bnodes(-Graph:rdf_graph, -Count:nonneg) is nondet.

rdf_number_of_bnodes(G, N):-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_bnode(G, _), N).



%! rdf_number_of_datatype_iris(-Count:nonneg) is det.

rdf_number_of_datatype_iris(N):-
  aggregate_all(count, rdf_datatype_iri(_), N).


%! rdf_number_of_datatype_iris(+Graph:rdf_graph, -Count:nonneg) is det.
%! rdf_number_of_datatype_iris(-Graph:rdf_graph, -Count:nonneg) is nondet.

rdf_number_of_datatype_iris(G, N):-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_datatype_iri(G, _), N).



%! rdf_number_of_instances(+Class:rdf_term, -Count:nonneg) is det.

rdf_number_of_instances(C, N):-
  aggregate_all(count, rdfs_individual_of(_, C), N).



%! rdf_number_of_objects(-Count:nonneg) is det.

rdf_number_of_objects(N):-
  aggregate_all(count, rdf_object(_), N).


%! rdf_number_of_objects(+Graph:rdf_graph, -Count:nonneg) is det.
%! rdf_number_of_objects(-Graph:rdf_graph, -Count:nonneg) is nondet.
% @throws exitence_error

rdf_number_of_objects(G, N):-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_object(G, _), N).


%! rdf_number_of_objects(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_objects(S, P, N):-
  rdf_number_of_objects(S, P, _, N).


%! rdf_number_of_objects(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Graph:rdf_graph,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_objects(S, P, G, N):-
  rdf_number_ofs0(O, S, P, O, G, N).



%! rdf_number_of_predicates(-Count:nonneg) is det.

rdf_number_of_predicates(N):-
  aggregate_all(count, rdf_predicate(_), N).


%! rdf_number_of_predicates(+Graph:rdf_graph, -Count:nonneg) is det.
%! rdf_number_of_predicates(-Graph:rdf_graph, -Count:nonneg) is nondet.

rdf_number_of_predicates(G, N):-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_predicate(G, _), N).


%! rdf_number_of_predicates(
%!   ?Subject:rdf_term,
%!   ?Object:rdf_term,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_predicates(S, O, N):-
  rdf_number_of_predicates(S, O, _, N).


%! rdf_number_of_predicates(
%!   ?Subject:rdf_term,
%!   ?Object:rdf_term,
%!   ?Graph:rdf_graph,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_predicates(S, O, G, N):-
  rdf_number_ofs0(P, S, P, O, G, N).



%! rdf_number_of_subjects(-Count:nonneg) is det.

rdf_number_of_subjects(N):-
  aggregate_all(count, rdf_subject(_), N).


%! rdf_number_of_subjects(+Graph:rdf_graph, -Count:nonneg) is det.
%! rdf_number_of_subjects(-Graph:rdf_graph, -Count:nonneg) is nondet.

rdf_number_of_subjects(G, N):-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_subject(G, _), N).


%! rdf_number_of_subjects(
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_subjects(P, O, N):-
  rdf_number_of_subjects(P, O, _, N).


%! rdf_number_of_subjects(
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:rdf_graph,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_subjects(P, O, G, N):-
  rdf_number_ofs0(S, S, P, O, G, N).



%! rdf_number_of_triples(-Count:nonneg) is det.

rdf_number_of_triples(N):-
  rdf_statistics0(triples(N)).


%! rdf_number_of_triples(+Graph:rdf_graph, -Count:nonneg) is det.
%! rdf_number_of_triples(-Graph:rdf_graph, -Count:nonneg) is nondet.
% @throws existence_error

rdf_number_of_triples(G, N):-
  rdf_graph_property(G, triples(N)).


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
%!   ?Graph:rdf_graph,
%!   -Count:nonneg
%! ) is det.

rdf_number_of_triples(S, P, O, G, N):-
  rdf_number_ofs0(rdf(S,P,O), S, P, O, G, N).





% HELPERS %

rdf_number_ofs0(Witness, S, P, O, G, N):-
  aggregate_all(count, distinct(Witness, rdf(S, P, O, G)), N).





% TESTS %

:- begin_tests(lod_stats).

test(rdf_number_of_subjects, [forall(test_case(G,N))]):-
  setup_call_cleanup(
    rdf_assert_graph(G),
    rdf_number_of_subjects(_, _, G, N0),
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
