:- module(
  q_dataset,
  [
    q_dataset/3,       % ?Dataset, -DefaultG, -NamedGs
    q_dataset_graph/2, % ?D, ?G
    q_dataset_ls/0
  ]
).

/** <module> Quine dataset

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(lists)).
:- use_module(library(persistency)).

:- initialization(db_attach('q_dataset.db', [])).

:- persistent
   q_dataset(dataset:atom, default:atom, named:atom).



%! q_dataset_graph(+D, +G) is semidet.
%! q_dataset_graph(+D, -G) is multi.
%! q_dataset_graph(-D, +G) is nondet.
%! q_dataset_graph(-D, -G) is nondet.

q_dataset_graph(D, G) :-
  q_dataset(D, DefaultG, NamedGs),
  member(G, [DefaultG|NamedGs]).



%! q_dataset_ls is det.

q_dataset_ls :-
  q_ls(q_dataset_graph).
