:- module(
  q_dataset_api,
  [
    q_dataset_label/2, % +D, -Lbl
    q_dataset_tree/4,  % ?M, +Order, +D, -Tree
    q_dataset_trees/3  % ?M, +Order, -Trees
  ]
).

/** <module> Quine: Dataset API

@author Wouter Beek
@version 2016/08-2016/10
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_dataset_db)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(tree/s_tree)).

:- rdf_meta
   q_dataset_label(r, -),
   q_dataset_tree(?, +, r, -).





%! q_dataset_label(+D, -Lbl) is det.

q_dataset_label(D, Str) :-
  q_dataset_named_graph(D, meta, MetaG),
  q_pref_label(hdt, D, Lit, MetaG),
  q_literal_string(Lit, Str).



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
  findall(G, q_dataset_graph(D, G), Gs),
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





% HELPERS %

q_dataset_tree_order0(Order, KeyDict, Trees, OrderedTrees) :-
  pairs_keys_values(Pairs, KeyDict.Order, Trees),
  desc_pairs_values(Pairs, OrderedTrees).
