:- module(
  rdf_stat,
  [
    rdf_descr_size/2,              % +S, -N
    rdf_number_of_bnodes/1,        % -N
    rdf_number_of_bnodes/2,        % ?G, -N
    rdf_number_of_datatype_iris/1, % -N
    rdf_number_of_datatype_iris/2, % ?G, -N
    rdf_number_of_objects/1,       % -N
    rdf_number_of_objects/2,       % ?G, -N
    rdf_number_of_objects/3,       % ?S, ?P, -N
    rdf_number_of_objects/4,       % ?S, ?P, ?G, -N
    rdf_number_of_predicates/1,    % -N
    rdf_number_of_predicates/2,    % ?G, -N
    rdf_number_of_predicates/3,    % ?S, ?O, -N
    rdf_number_of_predicates/4,    % ?S, ?O, ?G, -N
    rdf_number_of_subjects/1,      % -N
    rdf_number_of_subjects/2,      % ?G, -N
    rdf_number_of_subjects/3,      % ?P, ?O, -N
    rdf_number_of_subjects/4,      % ?P, ?O, ?G, -N
    rdf_number_of_triples/1,       % -N
    rdf_number_of_triples/2,       % ?G, -N
    rdf_number_of_triples/4,       % ?S, ?P, ?O, -N
    rdf_number_of_triples/5,       % ?S, ?P, ?O, ?G, -N
    rdf_rows_for_predicate/2       % +P, -Rows
  ]
).

/** <module> RDF statistics

@author Wouter Beek
@version 2015/08, 2015/10, 2015/12-2016/01, 2016/05-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   rdf_descr_size(r, -),
   rdf_number_of_bnodes(r, -),
   rdf_number_of_datatype_iris(r, -),
   rdf_number_of_objects(r, -),
   rdf_number_of_objects(r, r, -),
   rdf_number_of_objects(r, r, ?, -),
   rdf_number_of_predicates(r, -),
   rdf_number_of_predicates(r, o, -),
   rdf_number_of_predicates(r, o, ?, -),
   rdf_number_of_subjects(r, -),
   rdf_number_of_subjects(r, o, -),
   rdf_number_of_subjects(r, o, ?, -),
   rdf_number_of_triples(r, -),
   rdf_number_of_triples(r, r, o, -),
   rdf_number_of_triples(r, r, o, ?, -),
   rdf_rows_for_predicate(r, -).





%! rdf_descr_size(+S, -N) is det.

rdf_descr_size(S, N) :-
  rdf_number_of_triples(S, _, _, N).



%! rdf_number_of_bnodes(-N) is det.
%! rdf_number_of_bnodes(?G, -N) is det.

rdf_number_of_bnodes(N) :-
  aggregate_all(count, rdf_bnode(_), N).


rdf_number_of_bnodes(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_bnode(_, G), N).



%! rdf_number_of_datatype_iris(-N) is det.
%! rdf_number_of_datatype_iris(?G, -N) is det.

rdf_number_of_datatype_iris(N) :-
  aggregate_all(count, rdf_datatype_iri(_), N).


rdf_number_of_datatype_iris(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_datatype_iri(_, G), N).



%! rdf_number_of_objects(-N) is det.
%! rdf_number_of_objects(?G, -N) is det.
%! rdf_number_of_objects(?S, ?P, -N) is det.
%! rdf_number_of_objects(?S, ?P, ?G, -N) is det.

rdf_number_of_objects(N) :-
  aggregate_all(count, rdf_object(_), N).


rdf_number_of_objects(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_object(_, G), N).


rdf_number_of_objects(S, P, N) :-
  rdf_number_of_objects(S, P, _, N).


rdf_number_of_objects(S, P, G, N) :-
  rdf_number_ofs(O, S, P, O, G, N).



%! rdf_number_of_predicates(-N) is det.
%! rdf_number_of_predicates(?G, -N) is det.
%! rdf_number_of_predicates(?S, ?O, -N) is det.
%! rdf_number_of_predicates(?S, ?O, ?G, -N) is det.

rdf_number_of_predicates(N) :-
  aggregate_all(count, rdf_predicate(_), N).


rdf_number_of_predicates(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_predicate(_, G), N).


rdf_number_of_predicates(S, O, N) :-
  rdf_number_of_predicates(S, O, _, N).


rdf_number_of_predicates(S, O, G, N) :-
  rdf_number_ofs(P, S, P, O, G, N).



%! rdf_number_of_subjects(-N) is det.
%! rdf_number_of_subjects(?G, -N) is det.
%! rdf_number_of_subjects(?P, ?O, -N) is det.
%! rdf_number_of_subjects(?P, ?O, ?G, -N) is det.

rdf_number_of_subjects(N) :-
  aggregate_all(count, rdf_subject(_), N).


rdf_number_of_subjects(G, N) :-
  rdf_expect_graph(G),
  aggregate_all(count, rdf_subject(_, G), N).


rdf_number_of_subjects(P, O, N) :-
  rdf_number_of_subjects(P, O, _, N).


rdf_number_of_subjects(P, O, G, N) :-
  rdf_number_ofs(S, S, P, O, G, N).



%! rdf_number_of_triples(-N) is det.
%! rdf_number_of_triples(?G, -N) is det.
%! rdf_number_of_triples(?S, ?P, ?O, -N) is det.
%! rdf_number_of_triples(?S, ?P, ?O, ?G, -N) is det.

rdf_number_of_triples(N) :-
  rdf_statistics(triples(N)).


rdf_number_of_triples(G, N) :-
  rdf_graph_property(G, triples(N)).


rdf_number_of_triples(S, P, O, N) :-
  rdf_number_of_triples(S, P, O, _, N).


rdf_number_of_triples(S, P, O, G, N) :-
  rdf_number_ofs(rdf(S,P,O), S, P, O, G, N).



%! rdf_rows_for_predicate(+P, -Rows) is det.

rdf_rows_for_predicate(P, Rows) :-
  aggregate_all(set(O), rdf(_, P, O), Os),
  maplist(rdf_number_of_subjects(P), Os, Ns),
  pairs_keys_values(Pairs, Ns, Os),
  keysort(Pairs, SortedPairs),
  maplist(pair_inv_list, SortedPairs, Rows).





% HELPERS %

rdf_number_ofs(Witness, S, P, O, G, N) :-
  aggregate_all(count, distinct(Witness, rdf(S, P, O, G)), N).
