:- module(
  rdf_stats,
  [
    rdf_descr_size/2,              % +S, -Count
    rdf_number_of_bnodes/1,        % -Count
    rdf_number_of_bnodes/2,        % ?G, -Count
    rdf_number_of_datatype_iris/1, % -Count
    rdf_number_of_datatype_iris/2, % ?G, -Count
    rdf_number_of_objects/1,       % -Count
    rdf_number_of_objects/2,       % ?G, -Count
    rdf_number_of_objects/3,       % ?S, ?P, -Count
    rdf_number_of_objects/4,       % ?S, ?P, ?G, -Count
    rdf_number_of_predicates/1,    % -Count
    rdf_number_of_predicates/2,    % ?G, -Count
    rdf_number_of_predicates/3,    % ?S, ?O, -Count
    rdf_number_of_predicates/4,    % ?S, ?O, ?G, -Count
    rdf_number_of_subjects/1,      % -Count
    rdf_number_of_subjects/2,      % ?G, -Count
    rdf_number_of_subjects/3,      % ?P, ?O, -Count
    rdf_number_of_subjects/4,      % ?P, ?O, ?G, -Count
    rdf_number_of_triples/1,       % -Count
    rdf_number_of_triples/2,       % ?G, -Count
    rdf_number_of_triples/4,       % ?S, ?P, ?O, -Count
    rdf_number_of_triples/5        % ?S, ?P, ?O, ?G, -Count
  ]
).

/** <module> RDF: Statistics

@author Wouter Beek
@version 2015/08, 2015/10, 2015/12-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(plunit)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   rdf_descr_size(r, -),
   rdf_number_of_bnodes(r, -),
   rdf_number_of_datatype_iris(r, -),
   rdf_number_of_objects(r, r, -),
   rdf_number_of_objects(r, r, ?, -),
   rdf_number_of_predicates(r, o, -),
   rdf_number_of_predicates(r, o, ?, -),
   rdf_number_of_subjects(r, o, -),
   rdf_number_of_subjects(r, o, ?, -),
   rdf_number_of_triples(r, r, o, -),
   rdf_number_of_triples(r, r, o, ?, -).





%! rdf_descr_size(+S, -Count:nonneg) is det.

rdf_descr_size(S, N) :-
  rdf_number_of_triples(S, _, _, N).



%! rdf_number_of_bnodes(-Count:nonneg) is det.

rdf_number_of_bnodes(N) :-
  aggregate_all(count, rdf_bnode(_), N).


%! rdf_number_of_bnodes(+G, -Count:nonneg) is det.
%! rdf_number_of_bnodes(-G, -Count:nonneg) is nondet.

rdf_number_of_bnodes(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_bnode(G, _), N).



%! rdf_number_of_datatype_iris(-Count:nonneg) is det.

rdf_number_of_datatype_iris(N) :-
  aggregate_all(count, rdf_datatype_iri(_), N).


%! rdf_number_of_datatype_iris(+G, -Count:nonneg) is det.
%! rdf_number_of_datatype_iris(-G, -Count:nonneg) is nondet.

rdf_number_of_datatype_iris(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_datatype_iri(G, _), N).



%! rdf_number_of_objects(-Count:nonneg) is det.

rdf_number_of_objects(N) :-
  aggregate_all(count, rdf_object(_), N).


%! rdf_number_of_objects(+G, -Count:nonneg) is det.
%! rdf_number_of_objects(-G, -Count:nonneg) is nondet.
% @throws exitence_error

rdf_number_of_objects(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_object(G, _), N).


%! rdf_number_of_objects(?S, ?P, -Count:nonneg) is det.

rdf_number_of_objects(S, P, N) :-
  rdf_number_of_objects(S, P, _, N).


%! rdf_number_of_objects(?S, ?P, ?G, -Count:nonneg) is det.

rdf_number_of_objects(S, P, G, N) :-
  rdf_number_ofs(O, S, P, O, G, N).



%! rdf_number_of_predicates(-Count:nonneg) is det.

rdf_number_of_predicates(N) :-
  aggregate_all(count, rdf_predicate(_), N).


%! rdf_number_of_predicates(+G, -Count:nonneg) is det.
%! rdf_number_of_predicates(-G, -Count:nonneg) is nondet.

rdf_number_of_predicates(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_predicate(G, _), N).


%! rdf_number_of_predicates(?S, ?O, -Count:nonneg) is det.

rdf_number_of_predicates(S, O, N) :-
  rdf_number_of_predicates(S, O, _, N).


%! rdf_number_of_predicates(?S, ?O, ?G, -Count:nonneg) is det.

rdf_number_of_predicates(S, O, G, N) :-
  rdf_number_ofs(P, S, P, O, G, N).



%! rdf_number_of_subjects(-Count:nonneg) is det.

rdf_number_of_subjects(N) :-
  aggregate_all(count, rdf_subject(_), N).


%! rdf_number_of_subjects(+G, -Count:nonneg) is det.
%! rdf_number_of_subjects(-G, -Count:nonneg) is nondet.

rdf_number_of_subjects(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_subject(G, _), N).


%! rdf_number_of_subjects(?P, ?O, -Count:nonneg) is det.

rdf_number_of_subjects(P, O, N) :-
  rdf_number_of_subjects(P, O, _, N).


%! rdf_number_of_subjects(?P, ?O, ?G, -Count:nonneg) is det.

rdf_number_of_subjects(P, O, G, N) :-
  rdf_number_ofs(S, S, P, O, G, N).



%! rdf_number_of_triples(-Count:nonneg) is det.

rdf_number_of_triples(N) :-
  rdf_statistics(triples(N)).


%! rdf_number_of_triples(+G, -Count:nonneg) is det.
%! rdf_number_of_triples(-G, -Count:nonneg) is nondet.
% @throws existence_error

rdf_number_of_triples(G, N) :-
  rdf_graph_property(G, triples(N)).


%! rdf_number_of_triples(?S, ?P, ?O, -Count:nonneg) is det.

rdf_number_of_triples(S, P, O, N) :-
  rdf_number_of_triples(S, P, O, _, N).


%! rdf_number_of_triples(?S, ?P, ?O, ?G, -Count:nonneg) is det.

rdf_number_of_triples(S, P, O, G, N) :-
  rdf_number_ofs(rdf(S,P,O), S, P, O, G, N).





% HELPERS %

rdf_number_ofs(Witness, S, P, O, G, N) :-
  aggregate_all(count, distinct(Witness, rdf(S, P, O, G)), N).





% TESTS %

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
