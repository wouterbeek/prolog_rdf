:- module(
  rdf_stat,
  [
    rdf_number_of_bnodes/3,     % +M,             ?G, -NumBs
    rdf_number_of_classes/3,    % +M,             ?G, -NumCs
    rdf_number_of_datatypes/3,  % +M,             ?G, -NumDs
    rdf_number_of_domains/3,    % +M,     ?P,         -NumDoms
    rdf_number_of_domains/4,    % +M,     ?P,     ?G, -NumDoms
    rdf_number_of_instances/3,  % +M, ?C,             -NumIs
    rdf_number_of_instances/4,  % +M, ?C,         ?G, -NumIs
    rdf_number_of_literals/3,   % +M,             ?G, -NumLits
    rdf_number_of_objects/3,    % +M,             ?G, -NumOs
    rdf_number_of_objects/4,    % +M, ?S, ?P,         -NumOs
    rdf_number_of_objects/5,    % +M, ?S, ?P,     ?G, -NumOs
    rdf_number_of_predicates/3, % +M,             ?G, -NumPs
    rdf_number_of_predicates/4, % +M, ?S,     ?O,     -NumPs
    rdf_number_of_predicates/5, % +M, ?S,     ?O, ?G, -NumPs
    rdf_number_of_properties/3, % +M,             ?G, -NumProps
    rdf_number_of_quads/2,      % +M,                 -NumQuads
    rdf_number_of_ranges/3,     % +M,     ?P,         -NumRans
    rdf_number_of_ranges/4,     % +M,     ?P,     ?G, -NumRans
    rdf_number_of_subjects/3,   % +M,             ?G, -NumSs
    rdf_number_of_subjects/4,   % +M,     ?P, ?O,     -NumSs
    rdf_number_of_subjects/5,   % +M,     ?P, ?O, ?G, -NumSs
    rdf_number_of_triples/3,    % +M,             ?G, -NumTriples
    rdf_number_of_triples/5,    % +M, ?S, ?P, ?O,     -NumTriples
    rdf_number_of_triples/6     % +M, ?S, ?P, ?O, ?G, -NumTriples
  ]
).

/** <module> Quine statistics

@author Wouter Beek
@version 2016/06-2017/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdfs/rdfs_api)).
:- use_module(library(solution_sequences)).
:- use_module(library(trp/trp_stat)).
:- use_module(library(trp/trp_term)).

:- rdf_meta
   rdf_number_of_bnodes(+, r, -),
   rdf_number_of_classes(+, -),
   rdf_number_of_classes(+, r, -),
   rdf_number_of_datatype(+, r, -),
   rdf_number_of_domains(+, r, -),
   rdf_number_of_domains(+, r, r, -),
   rdf_number_of_instances(+, r, -),
   rdf_number_of_instances(+, r, r, -),
   rdf_number_of_literals(+, r, -),
   rdf_number_of_objects(+, r, -),
   rdf_number_of_objects(+, r, r, -),
   rdf_number_of_objects(+, r, r, r, -),
   rdf_number_of_predicates(+, r, -),
   rdf_number_of_predicates(+, r, o, -),
   rdf_number_of_predicates(+, r, o, r, -),
   rdf_number_of_properties(+, r, -),
   rdf_number_of_ranges(+, r, -),
   rdf_number_of_ranges(+, r, r, -),
   rdf_number_of_subjects(+, r, -),
   rdf_number_of_subjects(+, r, o, -),
   rdf_number_of_subjects(+, r, o, r, -),
   rdf_number_of_triples(+, r, -),
   rdf_number_of_triples(+, r, r, o, -),
   rdf_number_of_triples(+, r, r, o, r, -).





%! rdf_number_of_bnodes(+M, ?G, -NumBs) is nondet.
%
% The number of distinct blank nodes in graph G.

rdf_number_of_bnodes(M, G, NumBs) :-
  q_view_graph(M, G),
  aggregate_all(count, rdf_bnode(M, _, G), NumBs).



%! rdf_number_of_classes(+M, -NumCs) is det.
%! rdf_number_of_classes(+M, ?G, -NumCs) is det.

rdf_number_of_classes(M, NumCs) :-
  aggregate_all(count, distinct(C, rdf_class(M, C, _)), NumCs).


rdf_number_of_classes(M, G, NumCs) :-
  aggregate_all(count, distinct(C, rdf_class(M, C, G)), NumCs).



%! rdf_number_of_datatypes(+M, -NumDs) is det.
%! rdf_number_of_datatypes(+M, ?G, -NumDs) is det.

rdf_number_of_datatypes(M, NumDs) :-
  aggregate_all(count, rdf_datatype(M, _, _), NumDs).


rdf_number_of_datatypes(M, G, NumDs) :-
  q_view_graph(M, G),
  aggregate_all(count, rdf_datatype(M, _, G), NumDs).



%! rdf_number_of_domains(+M, ?P, -NumDoms) is det.
%! rdf_number_of_domains(+M, ?P, ?G, -NumDoms) is det.
%
% Enumerates the pairs of predicates P and their number of ranges.

rdf_number_of_domains(M, P, NumDoms) :-
  rdf_predicate(M, P),
  rdf_number_of_domains(M, P, _, NumDoms).


rdf_number_of_domains(M, P, G, NumDoms) :-
  rdf_predicate(M, P, G),
  aggregate_all(count, rdf_domain(M, P, _, G), NumDoms).



%! rdf_number_of_instances(+M, ?C, -NumIs) is nondet.
%! rdf_number_of_instances(+M, ?C, ?G, -NumIs) is nondet.
%
% Is able to iterate over classes, returning the number of instances
% per class.

rdf_number_of_instances(M, C, NumIs) :-
  rdf_class(M, C),
  aggregate_all(count, rdf_instance(M, _, C, _), NumIs).


rdf_number_of_instances(M, C, G, NumIs) :-
  rdf_class(M, C, G),
  aggregate_all(count, rdf_instance(M, _, C, G), NumIs).



%! rdf_number_of_literals(+M, ?G, -NumLits) is det.

rdf_number_of_literals(M, G, NumLits) :-
  aggregate_all(count, (rdf_object(M, O, G), rdf_is_literal(O)), NumLits).
  


%! rdf_number_of_objects(+M, ?G, -NumOs) is det.
%
% The number of distinct object terms that appear in graph G.

rdf_number_of_objects(M, G, NumOs) :-
  q_view_graph(hdt, G),
  (   M == hdt
  ->  hdt_number_of_objects(G, NumOs)
  ;   M == trp
  ->  aggregate_all(count, trp_object(_, G), NumOs)
  ).


%! rdf_number_of_objects(+M, ?S, ?P, -NumOs) is det.
%! rdf_number_of_objects(+M, ?S, ?P, ?G, -NumOs) is det.

rdf_number_of_objects(M, S, P, NumOs) :-
  rdf_number_of_objects(M, S, P, _, NumOs).


rdf_number_of_objects(M, S, P, G, NumOs) :-
  rdf_number_ofs0(M, O, S, P, O, G, NumOs).



%! rdf_number_of_predicates(+M, ?G, -NumPs) is det.
%
% The number of distinct predicate terms in graph G.

rdf_number_of_predicates(M, G, NumPs) :-
  q_view_graph(M, G),
  (   M == hdt
  ->  hdt_number_of_properties(G, NumPs)
  ;   M == trp
  ->  trp_number_of_predicates(G, NumPs)
  ).


%! rdf_number_of_predicates(+M, ?S, ?O, -NumPs) is det.
%! rdf_number_of_predicates(+M, ?S, ?O, ?G, -NumPs) is det.

rdf_number_of_predicates(M, S, O, NumPs) :-
  rdf_number_of_predicates(M, S, O, _, NumPs).


rdf_number_of_predicates(M, S, O, G, NumPs) :-
  rdf_number_ofs0(M, P, S, P, O, G, NumPs).



%! rdf_number_of_properties(+M, ?G, -NumProps) is det.
%
% The distinct number of properties in graph G.

rdf_number_of_properties(M, G, NumProps) :-
  aggregate_all(count, rdf_property(M, _, G), NumProps).



%! rdf_number_of_ranges(+M, ?P, -NumRans) is det.
%! rdf_number_of_ranges(+M, ?P, ?G, -NumRans) is det.
%
% Enumerates the pairs of predicates P and their number of distinct
% range classes.

rdf_number_of_ranges(M, P, NumRans) :-
  rdf_predicate(M, P),
  rdf_number_of_ranges(M, P, _, NumRans).


rdf_number_of_ranges(M, P, G, NumRans) :-
  rdf_predicate(M, P, G),
  aggregate_all(count, rdf_range(M, P, _, G), NumRans).



%! rdf_number_of_subjects(+M, ?G, -NumSs) is det.
%
% The number of distinct subject terms that appear in the subject
% position of some triple in graph G.

rdf_number_of_subjects(M, G, NumSs) :-
  q_view_graph(M, G),
  (   M == hdt
  ->  hdt_number_of_subjects(G, NumSs)
  ;   M == trp
  ->  aggregate_all(count, trp_subject(_, G), NumSs)
  ).


%! rdf_number_of_subjects(+M, ?P, ?O, -NumSs) is det.
%! rdf_number_of_subjects(+M, ?P, ?O, ?G, -NumSs) is det.

rdf_number_of_subjects(M, P, O, NumSs) :-
  rdf_number_of_subjects(M, P, O, _, NumSs).


rdf_number_of_subjects(M, P, O, G, NumSs) :-
  rdf_number_ofs0(M, S, S, P, O, G, NumSs).



%! rdf_number_of_quads(+M, -NumQuads) is det.
%
% The number of distinct quadruples.  This is calculated by summing
% the number of triples for each graph.

rdf_number_of_quads(M, NumQuads) :-
  aggregate_all(
    sum(NumTriples),
    rdf_number_of_triples(M, _, NumTriples),
    NumQuads
  ).



%! rdf_number_of_triples(+M, ?G, -NumTriples) is det.
%
% Enumerates graphs G and the number of triples in them.

rdf_number_of_triples(hdt, G, NumTriples) :-
  hdt_number_of_triples(G, NumTriples), !.
rdf_number_of_triples(trp, G, NumTriples) :-
  trp_number_of_triples(G, NumTriples).


%! rdf_number_of_triples(+M, ?S, ?P, ?O, -NumTriples) is det.
%! rdf_number_of_triples(+M, ?S, ?P, ?O, ?G, -NumTriples) is det.

rdf_number_of_triples(M, S, P, O, NumTriples) :-
  rdf_number_of_triples(M, S, P, O, _, NumTriples).


rdf_number_of_triples(M, S, P, O, G, NumTriples) :-
  rdf_number_ofs0(M, rdf(S,P,O), S, P, O, G, NumTriples).





% HELPERS %

%! rdf_number_ofs0(+M, +Witness, ?S, ?P, ?O, ?G, -NumWitnesses) is semidet.
%
% Master predicate that calculates how many instances match
% an〈S,P,O,G〉 pattern.

rdf_number_ofs0(M, Witness, S, P, O, G, NumWitnesses) :-
  aggregate_all(count, distinct(Witness, t(M, S, P, O, G)), NumWitnesses).
