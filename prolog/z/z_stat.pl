:- module(
  z_stat,
  [
    z_number_of_bnodes/1,     % -N
    z_number_of_bnodes/2,     % ?G, -N
    z_number_of_datatypes/1,  % -N
    z_number_of_datatypes/2,  % ?G, -N
    z_number_of_objects/1,    % -N
    z_number_of_objects/2,    % ?G, -N
    z_number_of_objects/3,    % ?S, ?P, -N
    z_number_of_objects/4,    % ?S, ?P, ?G, -N
    z_number_of_predicates/1, % -N
    z_number_of_predicates/2, % ?G, -N
    z_number_of_predicates/3, % ?S, ?O, -N
    z_number_of_predicates/4, % ?S, ?O, ?G, -N
    z_number_of_subjects/1,   % -N
    z_number_of_subjects/2,   % ?G, -N
    z_number_of_subjects/3,   % ?P, ?O, -N
    z_number_of_subjects/4,   % ?P, ?O, ?G, -N
    z_number_of_triples/1,    % -N
    z_number_of_triples/2,    % ?G, -N
    z_number_of_triples/4,    % ?S, ?P, ?O, -N
    z_number_of_triples/5     % ?S, ?P, ?O, ?G, -N
  ]
).

/** <module> Z statistics

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(hdt/hdt_stat)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_stat)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(z/z_ext)).
:- use_module(library(z/z_term)).

:- rdf_meta
   z_number_of_bnodes(r, -),
   z_number_of_datatype_iris(r, -),
   z_number_of_objects(r, -),
   z_number_of_objects(r, r, -),
   z_number_of_objects(r, r, ?, -),
   z_number_of_predicates(r, -),
   z_number_of_predicates(r, o, -),
   z_number_of_predicates(r, o, ?, -),
   z_number_of_subjects(r, -),
   z_number_of_subjects(r, o, -),
   z_number_of_subjects(r, o, ?, -),
   z_number_of_triples(r, -),
   z_number_of_triples(r, r, o, -),
   z_number_of_triples(r, r, o, ?, -).





%! z_number_of_bnodes(-N) is det.
%! z_number_of_bnodes(?G, -N) is det.

z_number_of_bnodes(N) :-
  aggregate_all(count, z_bnode(_), N).


z_number_of_bnodes(G, N) :-
  z_graph(G),
  aggregate_all(count, z_bnode(_, G), N).



%! z_number_of_datatypes(-N) is det.
%! z_number_of_datatypes(?G, -N) is det.

z_number_of_datatypes(N) :-
  aggregate_all(count, z_datatype(_), N).


z_number_of_datatypes(G, N) :-
  z_graph(G),
  aggregate_all(count, z_datatype(_, G), N).



%! z_number_of_objects(-N) is det.
%! z_number_of_objects(?G, -N) is det.
%! z_number_of_objects(?S, ?P, -N) is det.
%! z_number_of_objects(?S, ?P, ?G, -N) is det.

z_number_of_objects(N) :-
  aggregate_all(count, z_object(_), N).


z_number_of_objects(G, N) :-
  z_graph(G),
  aggregate_all(count, z_object(_, G), N).


z_number_of_objects(S, P, N) :-
  z_number_of_objects(S, P, _, N).


z_number_of_objects(S, P, G, N) :-
  z_number_ofs(O, S, P, O, G, N).



%! z_number_of_predicates(-N) is det.
%! z_number_of_predicates(?G, -N) is det.
%! z_number_of_predicates(?S, ?O, -N) is det.
%! z_number_of_predicates(?S, ?O, ?G, -N) is det.

z_number_of_predicates(N) :-
  aggregate_all(count, z_predicate(_), N).


z_number_of_predicates(G, N) :-
  z_graph(G),
  aggregate_all(count, z_predicate(_, G), N).


z_number_of_predicates(S, O, N) :-
  z_number_of_predicates(S, O, _, N).


z_number_of_predicates(S, O, G, N) :-
  z_number_ofs(P, S, P, O, G, N).



%! z_number_of_subjects(-N) is det.
%! z_number_of_subjects(?G, -N) is det.
%! z_number_of_subjects(?P, ?O, -N) is det.
%! z_number_of_subjects(?P, ?O, ?G, -N) is det.

z_number_of_subjects(N) :-
  aggregate_all(count, z_subject(_), N).


z_number_of_subjects(G, N) :-
  z_graph(G),
  aggregate_all(count, z_subject(_, G), N).


z_number_of_subjects(P, O, N) :-
  z_number_of_subjects(P, O, _, N).


z_number_of_subjects(P, O, G, N) :-
  z_number_ofs(S, S, P, O, G, N).



%! z_number_of_triples(-N) is det.
%! z_number_of_triples(?G, -N) is det.
%! z_number_of_triples(?S, ?P, ?O, -N) is det.
%! z_number_of_triples(?S, ?P, ?O, ?G, -N) is det.

z_number_of_triples(N) :-
  rdf_number_of_triples(N1),
  aggregate_all(sum(N), hdt_number_of_triples(_, N), N2),
  N is N1 + N2.


z_number_of_triples(G, N) :-
  rdf_number_of_triples(G, N).
z_number_of_triples(G, N) :-
  hdt_number_of_triples(G, N).


z_number_of_triples(S, P, O, N) :-
  z_number_of_triples(S, P, O, _, N).


z_number_of_triples(S, P, O, G, N) :-
  z_number_ofs(rdf(S,P,O), S, P, O, G, N).





% HELPERS %

z_number_ofs(Witness, S, P, O, G, N) :-
  aggregate_all(count, distinct(Witness, z(S, P, O, G)), N).
