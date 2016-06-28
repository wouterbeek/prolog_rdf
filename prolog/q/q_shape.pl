:- module(
  q_shape,
  [
    q_root/2, % ?M, ?Root
    q_root/3, % ?M, ?Root, ?G
    q_tree/3, % ?M, ?Root, -Tree
    q_tree/4  % ?M, ?Root, ?G, -Tree
  ]
).

/** <module> Quine shapes

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).

:- rdf_meta
   q_root(?, r),
   q_root(?, r, r),
   q_tree(?, r, -),
   q_tree(?, r, r, -).





%! q_root(?M, ?Root) is nondet.
%! q_root(?M, ?Root, ?G) is nondet.

q_root(M, Root) :-
  q_root(M, Root, _).


q_root(M, Root, G) :-
  q_subject(M, Root, G),
  \+ q(M, _, _, Root, G).



%! q_tree(?M, ?Root, -Tree) is det.
%! q_tree(?M, ?Root, ?G, -Tree) is det.

q_tree(M, Root, Tree) :-
  q_tree(M, Root, _, Tree).


q_tree(M, Root, G, Tree) :-
  q_subject(M, Root, G),
  q_tree0(M, [Root], G, [], [], Tree).


q_tree0(_, [], _, _, Tree, Tree) :- !.
q_tree0(M, [H|T], G, Hist, Tree, Sol) :-
  memberchk(H, Hist), !,
  q_tree0(M, T, G, Hist, Tree, Sol).
q_tree0(M, [S|T1], G, Hist1, Tree1, Sol) :-
  q_triples(M, S, _, _, G, Triples),
  aggregate_all(set(O), (member(rdf(_,_,O), Triples), q_is_subject(O)), New),
  ord_union(T1, New, T2),
  ord_add_element(Hist1, S, Hist2),
  ord_union(Tree1, Triples, Tree2),
  q_tree0(M, T2, G, Hist2, Tree2, Sol).
