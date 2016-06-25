:- module(
  z_shape,
  [
    z_tree/2, % ?S, -Tree
    z_tree/3  % ?S, ?G, -Tree
  ]
).

/** <module> Z shapes

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(z/z_stmt)).
:- use_module(library(z/z_term)).

:- rdf_meta
   z_tree(r, -),
   z_tree(r, r, -).





%! z_tree(?S, -Tree) is det.
%! z_tree(?S, ?G, -Tree) is det.

z_tree(S, Tree) :-
  z_tree(S, _, Tree).


z_tree(S, G, Tree) :-
  z_subject(S, G),
  z_tree0([S], G, [], [], Tree).


z_tree0([], _, _, Tree, Tree) :- !.
z_tree0([H|T], G, Hist, Tree, Sol) :-
  memberchk(H, Hist), !,
  z_tree0(T, G, Hist, Tree, Sol).
z_tree0([S|T1], G, Hist1, Tree1, Sol) :-
  z_triples(S, _, _, G, Triples),
  aggregate_all(set(O), (member(rdf(_,_,O), Triples), z_is_subject(O)), New),
  ord_union(T1, New, T2),
  ord_add_element(Hist1, S, Hist2),
  ord_union(Tree1, Triples, Tree2),
  z_tree0(T2, G, Hist2, Tree2, Sol).
