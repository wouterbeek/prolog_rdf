:- module(
  q_stat,
  [
    q_number_of_bnodes/2,     % +M,                 -NumBs
    q_number_of_bnodes/3,     % +M,             ?G, -NumBs
    q_number_of_datatypes/2,  % +M,                 -NumDs
    q_number_of_datatypes/3,  % +M,             ?G, -NumDs
    q_number_of_literals/2,   % +M,                 -NumLits
    q_number_of_literals/3,   % +M,             ?G, -NumLits
    q_number_of_objects/2,    % +M,                 -NumOs
    q_number_of_objects/3,    % +M,             ?G, -NumOs
    q_number_of_objects/4,    % +M, ?S, ?P,         -NumOs
    q_number_of_objects/5,    % +M, ?S, ?P,     ?G, -NumOs
    q_number_of_predicates/2, % +M,                 -NumPs
    q_number_of_predicates/3, % +M,             ?G, -NumPs
    q_number_of_predicates/4, % +M, ?S,     ?O,     -NumPs
    q_number_of_predicates/5, % +M, ?S,     ?O, ?G, -NumPs
    q_number_of_subjects/2,   % +M,                 -NumSs
    q_number_of_subjects/3,   % +M,             ?G, -NumSs
    q_number_of_subjects/4,   % +M,     ?P, ?O,     -NumSs
    q_number_of_subjects/5,   % +M,     ?P, ?O, ?G, -NumSs
    q_number_of_triples/2,    % +M,                 -NumTriples
    q_number_of_triples/3,    % +M,             ?G, -NumTriples
    q_number_of_triples/5,    % +M, ?S, ?P, ?O,     -NumTriples
    q_number_of_triples/6     % +M, ?S, ?P, ?O, ?G, -NumTriples
  ]
).

/** <module> Quine statistics

@author Wouter Beek
@version 2016/06-2016/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(rdf/rdf_stat)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   q_number_of_bnodes(+, r, -),
   q_number_of_datatype(+, r, -),
   q_number_of_literals(+, r, -),
   q_number_of_objects(+, r, -),
   q_number_of_objects(+, r, r, -),
   q_number_of_objects(+, r, r, r, -),
   q_number_of_predicates(+, r, -),
   q_number_of_predicates(+, r, o, -),
   q_number_of_predicates(+, r, o, r, -),
   q_number_of_subjects(+, r, -),
   q_number_of_subjects(+, r, o, -),
   q_number_of_subjects(+, r, o, r, -),
   q_number_of_triples(+, r, -),
   q_number_of_triples(+, r, r, o, -),
   q_number_of_triples(+, r, r, o, r, -).





%! q_number_of_bnodes(+M, -NumBs) is det.
%! q_number_of_bnodes(+M, ?G, -NumBs) is det.

q_number_of_bnodes(M, NumBs) :-
  aggregate_all(sum(N0), q_number_of_bnodes(M, _, N0), NumBs).


q_number_of_bnodes(M, G, NumBs) :-
  q_view_graph(M, G),
  aggregate_all(count, q_bnode(M, _, G), NumBs).



%! q_number_of_datatypes(+M, -NumDs) is det.
%! q_number_of_datatypes(+M, ?G, -NumDs) is det.

q_number_of_datatypes(M, NumDs) :-
  aggregate_all(sum(N0), q_datatype(M, _, N0), NumDs).


q_number_of_datatypes(M, G, NumDs) :-
  q_view_graph(M, G),
  aggregate_all(count, q_datatype(M, _, G), NumDs).



%! q_number_of_literals(+M, -NumLits) is det.
%! q_number_of_literals(+M, ?G, -NumLits) is det.

q_number_of_literals(M, NumLits) :-
  q_number_of_literals(M, _, NumLits).


q_number_of_literals(M, G, NumLits) :-
  aggregate_all(count, (q(M, _, _, O, G), q_is_literal(O)), NumLits).
  


%! q_number_of_objects(+M, -NumOs) is det.
%! q_number_of_objects(+M, ?G, -NumOs) is det.
%! q_number_of_objects(+M, ?S, ?P, -NumOs) is det.
%! q_number_of_objects(+M, ?S, ?P, ?G, -NumOs) is det.

q_number_of_objects(M, NumOs) :-
  aggregate_all(sum(N0), q_number_of_objects(M, _, N0), NumOs).


q_number_of_objects(hdt, G, NumOs) :- !,
  q_view_graph(hdt, G),
  hdt_number_of_objects(G, NumOs).
q_number_of_objects(trp, G, NumOs) :-
  aggregate_all(count, rdf_object(_, G), NumOs).


q_number_of_objects(M, S, P, NumOs) :-
  q_number_of_objects(M, S, P, _, NumOs).


q_number_of_objects(M, S, P, G, NumOs) :-
  q_number_ofs(M, O, S, P, O, G, NumOs).



%! q_number_of_predicates(+M, -NumPs) is det.
%! q_number_of_predicates(+M, ?G, -NumPs) is det.
%! q_number_of_predicates(+M, ?S, ?O, -NumPs) is det.
%! q_number_of_predicates(+M, ?S, ?O, ?G, -NumPs) is det.

q_number_of_predicates(hdt, NumPs) :- !,
  aggregate_all(sum(N0), q_number_of_predicates(hdt, _, N0), NumPs).
q_number_of_predicates(trp, NumPs) :-
  rdf_number_of_predicates(NumPs).


q_number_of_predicates(hdt, G, NumPs) :- !,
  q_view_graph(hdt, G),
  hdt_number_of_properties(G, NumPs).
q_number_of_predicates(trp, G, NumPs) :-
  rdf_number_of_predicates(G, NumPs).


q_number_of_predicates(M, S, O, NumPs) :-
  q_number_of_predicates(M, S, O, _, NumPs).


q_number_of_predicates(M, S, O, G, NumPs) :-
  q_number_ofs(M, P, S, P, O, G, NumPs).



%! q_number_of_subjects(+M, -NumSs) is det.
%! q_number_of_subjects(+M, ?G, -NumSs) is det.
%! q_number_of_subjects(+M, ?P, ?O, -NumSs) is det.
%! q_number_of_subjects(+M, ?P, ?O, ?G, -NumSs) is det.

q_number_of_subjects(M, NumSs) :-
  aggregate_all(sum(N0), q_number_of_subjects(M, _, N0), NumSs).


q_number_of_subjects(hdt, G, NumSs) :- !,
  q_view_graph(hdt, G),
  hdt_number_of_subjects(G, NumSs).
q_number_of_subjects(trp, G, NumSs) :-
  aggregate_all(count, rdf_subject(_, G), NumSs).


q_number_of_subjects(M, P, O, NumSs) :-
  q_number_of_subjects(M, P, O, _, NumSs).


q_number_of_subjects(M, P, O, G, NumSs) :-
  q_number_ofs(M, S, S, P, O, G, NumSs).



%! q_number_of_triples(+M, -NumTriples) is det.
%! q_number_of_triples(+M, ?G, -NumTriples) is det.
%! q_number_of_triples(+M, ?S, ?P, ?O, -NumTriples) is det.
%! q_number_of_triples(+M, ?S, ?P, ?O, ?G, -NumTriples) is det.

q_number_of_triples(hdt, NumTriples) :- !,
  aggregate_all(sum(N0), q_number_of_triples(hdt, _, N0), NumTriples).
q_number_of_triples(trp, NumTriples) :-
  rdf_number_of_triples(NumTriples).


q_number_of_triples(hdt, G, NumTriples) :- !,
  hdt_number_of_triples(G, NumTriples).
q_number_of_triples(trp, G, NumTriples) :-
  rdf_number_of_triples(G, NumTriples).


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
