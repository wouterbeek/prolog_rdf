:- module(
  q_dataset,
  [
    q_add_dataset/3,           % +D, +GRef, +Gs
    q_add_to_dataset/2,        % +D, +G
    q_dataset/1,               % ?D
    q_dataset/3,               % ?D, ?DefG, ?NGs
    q_dataset_default_graph/2, % ?D, ?DefG
    q_dataset_graph/2,         % ?D, ?G
    q_dataset_iri/2,           % ?Ref, ?D
    q_dataset_ls/0,
    q_dataset_named_graph/2,   % ?D, ?NG
    q_rm_dataset/1             % +D
  ]
).

/** <module> Quine dataset

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_term)).
:- use_module(library(persistency)).
:- use_module(library(semweb/rdf11)).

:- initialization(db_attach('q_dataset.db', [])).

:- persistent
   q_dataset(dataset:atom, default_graph:atom, named_graphs:list(atom)).

:- rdf_meta
   q_add_to_dataset(r, r),
   q_dataset(r),
   q_dataset_default_graph(r, r),
   q_dataset_graph(r, r),
   q_dataset_iri(?, r),
   q_dataset_named_graph(r, r),
   q_rm_dataset(r).





%! q_add_dataset(+D, +DefG, +Gs) is det.

q_add_dataset(D, DefG, Gs) :-
  with_mutex(q_dataset, assert_q_dataset(D, DefG, Gs)).



%! q_add_to_dataset(+D, +G) is det.

q_add_to_dataset(D, G) :-
  with_mutex(q_dataset, (
    retract_q_dataset(D, DefG, Gs1),
    ord_add_element(Gs1, G, Gs2),
    assert_q_dataset(D, DefG, Gs2)
  )).



%! q_dataset(+D) is semidet.
%! q_dataset(-D) is nondet.

q_dataset(D) :-
  q_dataset(D, _, _).



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
  q_alias_domain(ns, Domain),
  q_abox_iri(Domain, dataset, [Ref], D).



%! q_dataset_ls is det.

q_dataset_ls :-
  q_ls(q_dataset_graph).



%! q_dataset_named_graph(?D, ?NG) is nondet.

q_dataset_named_graph(D, NG) :-
  q_dataset(D, _, NGs),
  member(NG, NGs).



%! q_rm_dataset(+D) is det.

q_rm_dataset(D) :-
  with_mutex(q_dataset, retract_dataset(D, _, _)).
