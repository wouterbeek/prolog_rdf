:- module(
  rdf_isomorphism,
  [
    rdf_isomorphic_graphs/2 % +G1, +G2
  ]
).

/** <module> RDF isomorphism

@author Wouter Beek
@version 2016/03-2016/04
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_isomorphic_graphs(r, r).





%! rdf_isomorphic_graphs(+G1, +G2) is semidet.
% Succeeds if there is a consistent mapping between the blank nodes in
% both graphs / collections of tuples that makes both structures equal.
%
% This maps to the Prolog notion of *variant*, assuming a canonical
% ordering on tuples.

rdf_isomorphic_graphs(G1, G2) :-
  maplist(ensure_tuples0, [G1,G2], [Tuples1,Tuples2]),
  isomorphic_tuples0(Tuples1, Tuples2).


ensure_tuples0(G, Tuples) :-
  rdf_is_graph(G), !,
  rdf_graph_to_triples(G, Tuples).
ensure_tuples0(Tuples, Tuples) :-
  is_list(Tuples), !.
ensure_tuples0(G, _) :-
  type_error(rdf_graph, G).


isomorphic_tuples0(Tuples1, Tuples2) :-
  once(tuples_permutation0(Tuples1, Perm1)),
  % NONDET.
  tuples_permutation0(Tuples2, Perm2),
  variant(Perm1, Perm2), !.


tuples_permutation0(Tuples1, Perm) :-
  replace_bnodes_with_vars1(Tuples1, Tuples2),
  partition(ground, Tuples2, Ground, NonGround),
  sort(Ground, Sorted),
  append(Sorted, NonGroundPermutation, Perm),
  permutation(NonGround, NonGroundPermutation).


replace_bnodes_with_vars1(L1, L2) :-
  replace_bnodes_with_vars1(L1, [], L2).


replace_bnodes_with_vars1([], _, []).
replace_bnodes_with_vars1([H1|T1], Map1, [H2|T2]) :-
  replace_bnodes_with_vars2(H1, Map1, H2, Map2),
  replace_bnodes_with_vars1(T1, Map2, T2).


replace_bnodes_with_vars2(rdf(S1,P,O1), Map1, rdf(S2,P,O2), Map3) :- !,
  replace_bnode_with_var(S1, Map1, S2, Map2),
  replace_bnode_with_var(O1, Map2, O2, Map3).
replace_bnodes_with_vars2(rdf(S1,P,O1,G), Map1, rdf(S2,P,O2,G), Map3) :-
  replace_bnode_with_var(S1, Map1, S2, Map2),
  replace_bnode_with_var(O1, Map2, O2, Map3).


replace_bnode_with_var(B, Map, Var, Map) :-
  rdf_is_bnode(B),
  memberchk(B-Var, Map), !.
replace_bnode_with_var(B, Map, Var, [B-Var|Map]) :-
  rdf_is_bnode(B), !.
replace_bnode_with_var(T, Map, T, Map).
