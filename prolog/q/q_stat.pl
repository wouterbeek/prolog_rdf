:- module(
  q_stat,
  [
    q_number_of_bnodes/3,     % +M,             ?G, -NumBs
    q_number_of_classes/3,    % +M,             ?G, -NumCs
    q_number_of_datatypes/3,  % +M,             ?G, -NumDs
    q_number_of_domains/3,    % +M,     ?P,         -NumDoms
    q_number_of_domains/4,    % +M,     ?P,     ?G, -NumDoms
    q_number_of_instances/3,  % +M, ?C,             -NumIs
    q_number_of_instances/4,  % +M, ?C,         ?G, -NumIs
    q_number_of_literals/3,   % +M,             ?G, -NumLits
    q_number_of_objects/3,    % +M,             ?G, -NumOs
    q_number_of_objects/4,    % +M, ?S, ?P,         -NumOs
    q_number_of_objects/5,    % +M, ?S, ?P,     ?G, -NumOs
    q_number_of_predicates/3, % +M,             ?G, -NumPs
    q_number_of_predicates/4, % +M, ?S,     ?O,     -NumPs
    q_number_of_predicates/5, % +M, ?S,     ?O, ?G, -NumPs
    q_number_of_properties/3, % +M,             ?G, -NumProps
    q_number_of_quads/2,      % +M,                 -NumQuads
    q_number_of_ranges/3,     % +M,     ?P,         -NumRans
    q_number_of_ranges/4,     % +M,     ?P,     ?G, -NumRans
    q_number_of_subjects/3,   % +M,             ?G, -NumSs
    q_number_of_subjects/4,   % +M,     ?P, ?O,     -NumSs
    q_number_of_subjects/5,   % +M,     ?P, ?O, ?G, -NumSs
    q_number_of_triples/3,    % +M,             ?G, -NumTriples
    q_number_of_triples/5,    % +M, ?S, ?P, ?O,     -NumTriples
    q_number_of_triples/6     % +M, ?S, ?P, ?O, ?G, -NumTriples
  ]
).

/** <module> Quine statistics

@author Wouter Beek
@version 2016/06-2016/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(q/q_term)).
:- use_module(library(rdf/rdf_stat)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   q_number_of_bnodes(+, r, -),
   q_number_of_classes(+, -),
   q_number_of_classes(+, r, -),
   q_number_of_datatype(+, r, -),
   q_number_of_domains(+, r, -),
   q_number_of_domains(+, r, r, -),
   q_number_of_instances(+, r, -),
   q_number_of_instances(+, r, r, -),
   q_number_of_literals(+, r, -),
   q_number_of_objects(+, r, -),
   q_number_of_objects(+, r, r, -),
   q_number_of_objects(+, r, r, r, -),
   q_number_of_predicates(+, r, -),
   q_number_of_predicates(+, r, o, -),
   q_number_of_predicates(+, r, o, r, -),
   q_number_of_properties(+, r, -),
   q_number_of_ranges(+, r, -),
   q_number_of_ranges(+, r, r, -),
   q_number_of_subjects(+, r, -),
   q_number_of_subjects(+, r, o, -),
   q_number_of_subjects(+, r, o, r, -),
   q_number_of_triples(+, r, -),
   q_number_of_triples(+, r, r, o, -),
   q_number_of_triples(+, r, r, o, r, -).





%! q_number_of_bnodes(+M, ?G, -NumBs) is nondet.
%
% The number of distinct blank nodes in graph G.

q_number_of_bnodes(M, G, NumBs) :-
  q_view_graph(M, G),
  aggregate_all(count, q_bnode(M, _, G), NumBs).



%! q_number_of_classes(+M, -NumCs) is det.
%! q_number_of_classes(+M, ?G, -NumCs) is det.

q_number_of_classes(M, NumCs) :-
  aggregate_all(count, distinct(C, q_class(M, C, _)), NumCs).


q_number_of_classes(M, G, NumCs) :-
  aggregate_all(count, distinct(C, q_class(M, C, G)), NumCs).



%! q_number_of_datatypes(+M, -NumDs) is det.
%! q_number_of_datatypes(+M, ?G, -NumDs) is det.

q_number_of_datatypes(M, NumDs) :-
  aggregate_all(count, q_datatype(M, _, _), NumDs).


q_number_of_datatypes(M, G, NumDs) :-
  q_view_graph(M, G),
  aggregate_all(count, q_datatype(M, _, G), NumDs).



%! q_number_of_domains(+M, ?P, -NumDoms) is det.
%! q_number_of_domains(+M, ?P, ?G, -NumDoms) is det.
%
% Enumerates the pairs of predicates P and their number of ranges.

q_number_of_domains(M, P, NumDoms) :-
  q_predicate(M, P),
  q_number_of_domains(M, P, _, NumDoms).


q_number_of_domains(M, P, G, NumDoms) :-
  q_predicate(M, P, G),
  aggregate_all(count, q_domain(M, P, _, G), NumDoms).



%! q_number_of_instances(+M, ?C, -NumIs) is nondet.
%! q_number_of_instances(+M, ?C, ?G, -NumIs) is nondet.
%
% Is able to iterate over classes, returning the number of instances
% per class.

q_number_of_instances(M, C, NumIs) :-
  q_class(M, C),
  aggregate_all(count, q_instance(M, _, C, _), NumIs).


q_number_of_instances(M, C, G, NumIs) :-
  q_class(M, C, G),
  aggregate_all(count, q_instance(M, _, C, G), NumIs).



%! q_number_of_literals(+M, ?G, -NumLits) is det.

q_number_of_literals(M, G, NumLits) :-
  aggregate_all(count, (q_object(M, O, G), q_is_literal(O)), NumLits).
  


%! q_number_of_objects(+M, ?G, -NumOs) is det.
%
% The number of distinct object terms that appear in graph G.

q_number_of_objects(M, G, NumOs) :-
  q_view_graph(hdt, G),
  (   M == hdt
  ->  hdt_number_of_objects(G, NumOs)
  ;   M == trp
  ->  aggregate_all(count, rdf_object(_, G), NumOs)
  ).


%! q_number_of_objects(+M, ?S, ?P, -NumOs) is det.
%! q_number_of_objects(+M, ?S, ?P, ?G, -NumOs) is det.

q_number_of_objects(M, S, P, NumOs) :-
  q_number_of_objects(M, S, P, _, NumOs).


q_number_of_objects(M, S, P, G, NumOs) :-
  q_number_ofs(M, O, S, P, O, G, NumOs).



%! q_number_of_predicates(+M, ?G, -NumPs) is det.
%
% The number of distinct predicate terms in graph G.

q_number_of_predicates(M, G, NumPs) :-
  q_view_graph(M, G),
  (   M == hdt
  ->  hdt_number_of_properties(G, NumPs)
  ;   M == trp
  ->  rdf_number_of_predicates(G, NumPs)
  ).


%! q_number_of_predicates(+M, ?S, ?O, -NumPs) is det.
%! q_number_of_predicates(+M, ?S, ?O, ?G, -NumPs) is det.

q_number_of_predicates(M, S, O, NumPs) :-
  q_number_of_predicates(M, S, O, _, NumPs).


q_number_of_predicates(M, S, O, G, NumPs) :-
  q_number_ofs(M, P, S, P, O, G, NumPs).



%! q_number_of_properties(+M, ?G, -NumProps) is det.
%
% The distinct number of properties in graph G.

q_number_of_properties(M, G, NumProps) :-
  aggregate_all(count, q_property(M, _, G), NumProps).



%! q_number_of_ranges(+M, ?P, -NumRans) is det.
%! q_number_of_ranges(+M, ?P, ?G, -NumRans) is det.
%
% Enumerates the pairs of predicates P and their number of distinct
% range classes.

q_number_of_ranges(M, P, NumRans) :-
  q_predicate(M, P),
  q_number_of_ranges(M, P, _, NumRans).


q_number_of_ranges(M, P, G, NumRans) :-
  q_predicate(M, P, G),
  aggregate_all(count, q_range(M, P, _, G), NumRans).



%! q_number_of_subjects(+M, ?G, -NumSs) is det.
%
% The number of distinct subject terms that appear in the subject
% position of some triple in graph G.

q_number_of_subjects(M, G, NumSs) :-
  q_view_graph(M, G),
  (   M == hdt
  ->  hdt_number_of_subjects(G, NumSs)
  ;   M == trp
  ->  aggregate_all(count, rdf_subject(_, G), NumSs)
  ).


%! q_number_of_subjects(+M, ?P, ?O, -NumSs) is det.
%! q_number_of_subjects(+M, ?P, ?O, ?G, -NumSs) is det.

q_number_of_subjects(M, P, O, NumSs) :-
  q_number_of_subjects(M, P, O, _, NumSs).


q_number_of_subjects(M, P, O, G, NumSs) :-
  q_number_ofs(M, S, S, P, O, G, NumSs).



%! q_number_of_quads(+M, -NumQuads) is det.
%
% The number of distinct quadruples.  This is calculated by summing
% the number of triples for each graph.

q_number_of_quads(M, NumQuads) :-
  aggregate_all(
    sum(NumTriples),
    q_number_of_triples(M, _, NumTriples),
    NumQuads
  ).



%! q_number_of_triples(+M, ?G, -NumTriples) is det.
%
% Enumerates graphs G and the number of triples in them.

q_number_of_triples(hdt, G, NumTriples) :-
  hdt_number_of_triples(G, NumTriples), !.
q_number_of_triples(trp, G, NumTriples) :-
  rdf_number_of_triples(G, NumTriples).


%! q_number_of_triples(+M, ?S, ?P, ?O, -NumTriples) is det.
%! q_number_of_triples(+M, ?S, ?P, ?O, ?G, -NumTriples) is det.

q_number_of_triples(M, S, P, O, NumTriples) :-
  q_number_of_triples(M, S, P, O, _, NumTriples).


q_number_of_triples(M, S, P, O, G, NumTriples) :-
  q_number_ofs(M, rdf(S,P,O), S, P, O, G, NumTriples).





% HELPERS %

%! q_number_of0(+M, +Witness, ?S, ?P, ?O, ?G, -NumWitnesses) is semidet.
%
% Master predicate that calculates how many instances match an
% 〈S,P,O,G〉 pattern.

q_number_ofs(M, Witness, S, P, O, G, NumWitnesses) :-
  aggregate_all(count, distinct(Witness, q(M, S, P, O, G)), NumWitnesses).
