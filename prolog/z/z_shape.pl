:- module(
  z_shape,
  [
    z_root/2, % ?M, ?Root
    z_root/3, % ?M, ?Root, ?G
    z_tree/3, % ?M, ?Root, -Tree
    z_tree/4  % ?M, ?Root, ?G, -Tree
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
   z_root(?, r),
   z_root(?, r, r),
   z_tree(?, r, -),
   z_tree(?, r, r, -).





%! z_root(?M, ?Root) is nondet.
%! z_root(?M, ?Root, ?G) is nondet.

z_root(M, Root) :-
  z_root(M, Root, _).


z_root(M, Root, G) :-
  z_subject(M, Root, G),
  \+ z(M, _, _, Root, G).



%! z_tree(?M, ?Root, -Tree) is det.
%! z_tree(?M, ?Root, ?G, -Tree) is det.

z_tree(M, Root, Tree) :-
  z_tree(M, Root, _, Tree).


z_tree(M, Root, G, Tree) :-
  z_subject(M, Root, G),
  z_tree0(M, [Root], G, [], [], Tree).


z_tree0(_, [], _, _, Tree, Tree) :- !.
z_tree0(M, [H|T], G, Hist, Tree, Sol) :-
  memberchk(H, Hist), !,
  z_tree0(M, T, G, Hist, Tree, Sol).
z_tree0(M, [S|T1], G, Hist1, Tree1, Sol) :-
  z_triples(M, S, _, _, G, Triples),
  aggregate_all(set(O), (member(rdf(_,_,O), Triples), rdf_is_subject(O)), New),
  ord_union(T1, New, T2),
  ord_add_element(Hist1, S, Hist2),
  ord_union(Tree1, Triples, Tree2),
  z_tree0(M, T2, G, Hist2, Tree2, Sol).
