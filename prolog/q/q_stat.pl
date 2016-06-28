:- module(
  q_stat,
  [
    q_number_of_bnodes/2,     % ?M, -N
    q_number_of_bnodes/3,     % ?M, ?G, -N
    q_number_of_datatypes/2,  % ?M, -N
    q_number_of_datatypes/3,  % ?M, ?G, -N
    q_number_of_objects/2,    % ?M, -N
    q_number_of_objects/3,    % ?M, ?G, -N
    q_number_of_objects/4,    % ?M, ?S, ?P, -N
    q_number_of_objects/5,    % ?M, ?S, ?P, ?G, -N
    q_number_of_predicates/2, % ?M, -N
    q_number_of_predicates/3, % ?M, ?G, -N
    q_number_of_predicates/4, % ?M, ?S, ?O, -N
    q_number_of_predicates/5, % ?M, ?S, ?O, ?G, -N
    q_number_of_subjects/2,   % ?M, -N
    q_number_of_subjects/3,   % ?M, ?G, -N
    q_number_of_subjects/4,   % ?M, ?P, ?O, -N
    q_number_of_subjects/5,   % ?M, ?P, ?O, ?G, -N
    q_number_of_triples/2,    % ?M, -N
    q_number_of_triples/3,    % ?M, ?G, -N
    q_number_of_triples/5,    % ?M, ?S, ?P, ?O, -N
    q_number_of_triples/6     % ?M, ?S, ?P, ?O, ?G, -N
  ]
).

/** <module> Quine statistics

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(hdt/hdt_stat)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_ext)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(rdf/rdf_stat)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   q_number_of_bnodes(?, r, -),
   q_number_of_datatype(?, r, -),
   q_number_of_objects(?, r, -),
   q_number_of_objects(?, r, r, -),
   q_number_of_objects(?, r, r, ?, -),
   q_number_of_predicates(?, r, -),
   q_number_of_predicates(?, r, o, -),
   q_number_of_predicates(?, r, o, ?, -),
   q_number_of_subjects(?, r, -),
   q_number_of_subjects(?, r, o, -),
   q_number_of_subjects(?, r, o, ?, -),
   q_number_of_triples(?, r, -),
   q_number_of_triples(?, r, r, o, -),
   q_number_of_triples(?, r, r, o, ?, -).





%! q_number_of_bnodes(?M, -N) is det.
%! q_number_of_bnodes(?M, ?G, -N) is det.

q_number_of_bnodes(M, N) :-
  aggregate_all(count, q_bnode(M, _), N).


q_number_of_bnodes(M, G, N) :-
  q_graph(M, G),
  aggregate_all(count, q_bnode(M, _, G), N).



%! q_number_of_datatypes(?M, -N) is det.
%! q_number_of_datatypes(?M, ?G, -N) is det.

q_number_of_datatypes(M, N) :-
  aggregate_all(count, q_datatype(M, _), N).


q_number_of_datatypes(M, G, N) :-
  q_graph(M, G),
  aggregate_all(count, q_datatype(M, _, G), N).



%! q_number_of_objects(?M, -N) is det.
%! q_number_of_objects(?M, ?G, -N) is det.
%! q_number_of_objects(?M, ?S, ?P, -N) is det.
%! q_number_of_objects(?M, ?S, ?P, ?G, -N) is det.

q_number_of_objects(M, N) :-
  aggregate_all(count, q_object(M, _), N).


q_number_of_objects(M, G, N) :-
  q_graph(M, G),
  aggregate_all(count, q_object(M, _, G), N).


q_number_of_objects(M, S, P, N) :-
  q_number_of_objects(M, S, P, _, N).


q_number_of_objects(M, S, P, G, N) :-
  q_number_ofs(M, O, S, P, O, G, N).



%! q_number_of_predicates(?M, -N) is det.
%! q_number_of_predicates(?M, ?G, -N) is det.
%! q_number_of_predicates(?M, ?S, ?O, -N) is det.
%! q_number_of_predicates(?M, ?S, ?O, ?G, -N) is det.

q_number_of_predicates(M, N) :-
  aggregate_all(count, q_predicate(M, _), N).


q_number_of_predicates(M, G, N) :-
  q_graph(M, G),
  aggregate_all(count, q_predicate(M, _, G), N).


q_number_of_predicates(M, S, O, N) :-
  q_number_of_predicates(M, S, O, _, N).


q_number_of_predicates(M, S, O, G, N) :-
  q_number_ofs(M, P, S, P, O, G, N).



%! q_number_of_subjects(?M, -N) is det.
%! q_number_of_subjects(?M, ?G, -N) is det.
%! q_number_of_subjects(?M, ?P, ?O, -N) is det.
%! q_number_of_subjects(?M, ?P, ?O, ?G, -N) is det.

q_number_of_subjects(M, N) :-
  aggregate_all(count, q_subject(M, _), N).


q_number_of_subjects(M, G, N) :-
  q_graph(M, G),
  aggregate_all(count, q_subject(M, _, G), N).


q_number_of_subjects(M, P, O, N) :-
  q_number_of_subjects(M, P, O, _, N).


q_number_of_subjects(M, P, O, G, N) :-
  q_number_ofs(M, S, S, P, O, G, N).



%! q_number_of_triples(?M, -N) is det.
%! q_number_of_triples(?M, ?G, -N) is det.
%! q_number_of_triples(?M, ?S, ?P, ?O, -N) is det.
%! q_number_of_triples(?M, ?S, ?P, ?O, ?G, -N) is det.

q_number_of_triples(rdf, N) :-
  rdf_number_of_triples(N).
q_number_of_triples(hdt, N) :-
  aggregate_all(sum(N0), hdt_number_of_triples(_, N0), N).


q_number_of_triples(rdf, G, N) :-
  rdf_number_of_triples(G, N).
q_number_of_triples(hdt, G, N) :-
  hdt_number_of_triples(G, N).


q_number_of_triples(M, S, P, O, N) :-
  q_number_of_triples(M, S, P, O, _, N).


q_number_of_triples(M, S, P, O, G, N) :-
  q_number_ofs(M, rdf(S,P,O), S, P, O, G, N).





% HELPERS %

q_number_ofs(M, Witness, S, P, O, G, N) :-
  aggregate_all(count, distinct(Witness, q(M, S, P, O, G)), N).
