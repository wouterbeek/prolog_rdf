:- module(
  q_dataset,
  [
    q_add_to_dataset/2,        % +D, +G
    q_create_dataset/3,        % +DRef, +GRef, +Gs
    q_create_dataset/4,        % +DRef, +GRef, +Gs, -D
    q_dataset/1,               % ?D
    q_dataset/3,               % ?D, ?DefG, ?NGs
    q_dataset_default_graph/2, % ?D, ?DefG
    q_dataset_graph/2,         % ?D, ?G
    q_dataset_iri/2,           % ?Ref, ?D
    q_dataset_ls/0,
    q_dataset_named_graph/2,   % ?D, ?NG 
    q_dataset_tree/4,          % ?M, +Order, +D, -Tree
    q_dataset_trees/3,         % ?M, +Order, -Trees
    q_rm_dataset/0,
    q_rm_dataset/1             % +D
  ]
).

/** <module> Quine dataset

@author Wouter Beek
@version 2016/08-2016/09, 2016/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(persistency)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(tree/s_tree)).
:- use_module(library(uri)).

:- initialization(db_attach('q_dataset.db', [])).

:- persistent
   q_dataset(dataset:atom, default_graph:atom, named_graphs:list(atom)).

:- rdf_meta
   q_create_to_dataset(r, r),
   q_dataset(r),
   q_dataset_default_graph(r, r),
   q_dataset_graph(r, r),
   q_dataset_iri(?, r),
   q_dataset_named_graph(r, r),
   q_dataset_tree(+, r, -),
   q_rm_dataset(r).





%! q_add_to_dataset(+D, +G) is det.

q_add_to_dataset(D, G) :-
  with_mutex(q_dataset, (
    retractall_q_dataset(D, DefG, Gs1),
    ord_add_element(Gs1, G, Gs2),
    assert_q_dataset(D, DefG, Gs2)
  )).



%! q_create_dataset(+RefD, +DefG, +NGs) is det.
%! q_create_dataset(+RefD, +DefG, +NGs, -D) is det.

q_create_dataset(RefD, DefG, NGs) :-
  q_create_dataset(RefD, DefG, NGs, _).


q_create_dataset(RefD, DefG, NGs, D) :-
  q_dataset_iri(RefD, D),
  with_mutex(q_dataset, q_create_dataset_mutex(D, DefG, NGs)).

q_create_dataset_mutex(D, _, _) :-
  q_dataset(D, _, _), !.
q_create_dataset_mutex(D, DefG ,NGs) :-
  assert_q_dataset(D, DefG, NGs).



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
  q_alias_prefix(ns, Prefix),
  uri_components(Prefix, uri_components(Scheme,Auth,_,_,_)),
  q_abox_iri(Scheme, Auth, dataset, [Ref], D).



%! q_dataset_ls is det.

q_dataset_ls :-
  aggregate_all(D-G, q_dataset_graph(D, G), Pairs),
  (pairs_to_tree(Pairs, Tree) -> print_tree(Tree) ; writeln("∅")).



%! q_dataset_named_graph(?D, ?NG) is nondet.

q_dataset_named_graph(D, NG) :-
  q_dataset(D, _, NGs),
  member(NG, NGs).



%! q_dataset_tree(?M, +Order, +D, -Tree) is det.
%
% Return a dataset tree in which the graphs are ordered in one of the
% following ways:
%
%   - `lexicographic`: by graph name
%
%   - `number_of_triples`: by the number of triples per graph

q_dataset_tree(M, Order, D, Tree) :-
  q_dataset_tree0(M, Order, D, _, Tree).
  

q_dataset_tree0(M, Order, D, SumNumTriples, t(D,OrderedTrees)) :-
  findall(G, q_dataset_named_graph(D, G), Gs),
  maplist(q_graph_tree0(M), Gs, NumTriples, Trees),
  KeyDict = _{lexicographic: Gs, number_of_triples: NumTriples},
  q_dataset_tree_order0(Order, KeyDict, Trees, OrderedTrees),
  sum_list(NumTriples, SumNumTriples).


q_graph_tree0(M, G, NumTriples, t(G,[])) :-
  (var(M) -> once(q_view_graph(M, G)) ; true),
  q_number_of_triples(M, G, NumTriples).



%! q_dataset_trees(?M, +Order, -Trees) is det.
%
% Return all dataset trees ordered in one of the following ways:
%
%   - `lexicographic`: by dataset and graph name
%
%   - `number_of_triples`: by the number of triples per dataset and
%     graph

q_dataset_trees(M, Order, OrderedTrees) :-
  aggregate_all(set(D), q_dataset(D), Ds),
  maplist(q_dataset_tree0(M, Order), Ds, NumTriples, Trees),
  KeyDict = _{lexicographic: Ds, number_of_triples: NumTriples},
  q_dataset_tree_order0(Order, KeyDict, Trees, OrderedTrees).



%! q_rm_dataset is det.
%! q_rm_dataset(+D) is det.

q_rm_dataset :-
  forall(
    q_dataset(D),
    q_rm_dataset(D)
  ).


q_rm_dataset(D) :-
  retractall_q_dataset(D, _, _).



% HELPERS %

q_dataset_tree_order0(Order, KeyDict, Trees, OrderedTrees) :-
  pairs_keys_values(Pairs, KeyDict.Order, Trees),
  desc_pairs_values(Pairs, OrderedTrees).
