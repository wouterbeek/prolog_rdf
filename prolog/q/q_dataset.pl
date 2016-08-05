:- module(
  q_dataset,
  [
    q_add_dataset/3,           % +DRef, +NGRefs, -D
    q_dataset_default_graph/2, % ?D, ?DefG
    q_dataset_graph/2,         % ?D, ?G
    q_dataset_iri/2,           % ?Ref, ?D
    q_dataset_ls/0,
    q_dataset_named_graph/2    % ?D, ?NG
  ]
).

/** <module> Quine dataset

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(q/q_graph)).
:- use_module(library(q/q_iri)).
:- use_module(library(persistency)).

:- initialization(db_attach('q_dataset.db', [])).

:- persistent
   q_dataset(dataset:atom, default_graph:atom, named_graphs:list(atom)).





%! q_add_dataset(+DRef, +NGRefs, -D) is det.

q_add_dataset(DRef, NGRefs, D) :-
  q_dataset_iri(DRef, D),
  maplist(q_graph_iri, [DRef|NGRefs], [DefG|NGs]),
  assert_q_dataset(D, DefG, NGs).



%! q_dataset_default_graph(?D, ?DefG) is nondet.

q_dataset_default_graph(D, DefG) :-
  q_dataset(D, DefG, _).



%! q_dataset_graph(+D, +G) is semidet.
%! q_dataset_graph(+D, -G) is multi.
%! q_dataset_graph(-D, +G) is nondet.
%! q_dataset_graph(-D, -G) is nondet.

q_dataset_graph(D, G) :-
  q_dataset_default_graph(D, G).
q_dataset_graph(D, G) :-
  q_dataset_named_graph(D, G).



%! q_dataset_iri(+Ref, -D) is det.
%! q_dataset_iri(-Ref, +D) is det.

q_dataset_iri(Ref, D) :-
  q_abox_iri(ns, dataset, Ref, D).



%! q_dataset_ls is det.

q_dataset_ls :-
  q_ls(q_dataset_graph).



%! q_dataset_named_graph(?D, ?NG) is nondet.

q_dataset_named_graph(D, NG) :-
  q_dataset(D, _, NGs),
  member(NG, NGs).
