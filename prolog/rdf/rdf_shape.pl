:- module(
  rdf_shape,
  [
    rdf_cbd_quad/3,     % ?M, +Node,     -Quad
    rdf_cbd_quads/3,    % ?M, +Node,     -Quads
    rdf_cbd_triple/3,   % ?M, +Node,     -Triple
    rdf_cbd_triple/4,   % ?M, +Node, ?G, -Triple
    rdf_cbd_triples/3,  % ?M, ?Node,     -Triples
    rdf_cbd_triples/4,  % ?M, ?Node, ?G, -Triples
    rdf_root/2,         % ?M, ?Root
    rdf_root/3,         % ?M, ?Root, ?G
    rdf_scbd_quad/3,    % ?M, +Node,     -Quad
    rdf_scbd_quads/3,   % ?M, +Node,     -Quads
    rdf_scbd_triple/3,  % ?M, +Node,     -Triple
    rdf_scbd_triple/4,  % ?M, +Node, ?G, -Triple
    rdf_scbd_triples/3, % ?M, ?Node,     -Triples
    rdf_scbd_triples/4, % ?M, ?Node, ?G, -Triples
    rdf_tree_triple/3,  % ?M, ?Root,     -Triple
    rdf_tree_triple/4,  % ?M, ?Root, ?G, -Triple
    rdf_tree_triples/3, % ?M, ?Root,     -Triples
    rdf_tree_triples/4  % ?M, ?Root, ?G, -Triples
  ]
).

/** <module> Quine shapes

# (Symmmetric) Concise Bounded Description ((S)CBD) API

## Concise Bounded Description (CBD)

  1. Include in the subgraph all statements in the source graph where
     the subject of the statement is the starting node.

  2. Recursively, for all statements identified in the subgraph thus
     far having a blank node object, include in the subgraph all
     statements in the source graph where the subject of the statement
     is the blank node in question and which are not already included
     in the subgraph.

  3. Recursively, for all statements included in the subgraph thus
     far, for all reifications of each statement in the source graph,
     include the concise bounded description beginning from the
     `rdf:Statement' node of each reification.

This results in a subgraph where the object nodes are either URI
references, literals, or blank nodes not serving as the subject of any
statement in the graph.


## Symmetric Concise Bounded Description (SCBD)

  1. Include in the subgraph all statements in the source graph where
     the object of the statement is the starting node;

  2. Recursively, for all statements identified in the subgraph thus
     far having a blank node subject not equal to the starting node,
     include in the subgraph all statements in the source graph where
     the object of the statement is the blank node in question and
     which are not already included in the subgraph.

  3. Recursively, for all statements included in the subgraph thus
     far, for all reifications of each statement in the source graph,
     include the symmetric concise bounded description beginning from
     the rdf:Statement node of each reification.

  4. Include in the subgraph the concise bounded description beginning
     from the starting node.

---

@author Wouter Beek
@version 2016/06-2016/07, 2017/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_cbd_quad(?, o, -),
   rdf_cbd_quads(?, o, -),
   rdf_cbd_triple(?, o, -),
   rdf_cbd_triple(?, o, r, -),
   rdf_cbd_triples(?, o, -),
   rdf_cbd_triples(?, o, r, -),
   rdf_root(?, r),
   rdf_root(?, r, r),
   rdf_scbd_quad(?, o, -),
   rdf_scbd_quads(?, o, -),
   rdf_scbd_triple(?, o, -),
   rdf_scbd_triple(?, o, r, -),
   rdf_scbd_triples(?, o, -),
   rdf_scbd_triples(?, o, r, -),
   rdf_tree_triple(?, r, -),
   rdf_tree_triple(?, r, r, -),
   rdf_tree_triples(?, r, -),
   rdf_tree_triples(?, r, r, -).





%! rdf_cbd_quad(?M, +Node, -Quad) is nondet.
%! rdf_cbd_quad(?M, +Node, -Quad) is nondet.

rdf_cbd_quad(M, Node, rdf(S,P,O,G)) :-
  rdf_cbd_triple(M, Node, G, rdf(S,P,O)).


rdf_cbd_quads(M, Node, Quads) :-
  rdf_subject(M, Node),
  aggregate_all(set(Quad), rdf_cbd_quad(M, Node, Quad), Quads).



%! rdf_cbd_triple(?M, +Node, -Triple) is nondet.
%! rdf_cbd_triple(?M, +Node, ?G, -Triple) is nondet.
%! rdf_cbd_triples(?M, ?Node, -Triples) is det.
%! rdf_cbd_triples(?M, ?Node, ?G, -Triples) is det.

rdf_cbd_triple(M, Node, Triple) :-
  rdf_cbd_triple(M, Node, _, Triple).


rdf_cbd_triple(M, Node, G, Triple) :-
  rdf_cbd_triple0(M, Node, [Node], G, Triple).


rdf_cbd_triple0(M, S, Hist1, G, Triple) :-
  rdf_is_subject(S),
  t(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(O),
      node_history0(Hist1, O, Hist2),
      rdf_cbd_triple0(M, O, Hist2, G, Triple)
  ;   rdf_reification(M, S, P, O, G, Stmt),
      node_history0(Hist1, Stmt, Hist2),
      rdf_cbd_triple0(M, Stmt, Hist2, G, Triple)
  ).


rdf_cbd_triples(M, Node, Triples) :-
  rdf_cbd_triples(M, Node, _, Triples).


rdf_cbd_triples(M, Node, G, Triples) :-
  rdf_subject(M, Node, G),
  aggregate_all(set(Triple), rdf_cbd_triple(M, Node, G, Triple), Triples).



%! rdf_root(?M, ?Root) is nondet.
%! rdf_root(?M, ?Root, ?G) is nondet.

rdf_root(M, Root) :-
  rdf_root(M, Root, _).


rdf_root(M, Root, G) :-
  rdf_subject(M, Root, G),
  \+ t(M, _, _, Root, G).



%! rdf_scbd_quad(?M, +Node, -Quad) is nondet.
%! rdf_scbd_quads(?M, +Node, -Quads) is nondet.

rdf_scbd_quad(M, Node, rdf(S,P,O,G)) :-
  rdf_scbd_triple(M, Node, G, rdf(S,P,O)).


rdf_scbd_quads(M, Node, Quads) :-
  rdf_subject(M, Node),
  aggregate_all(set(Quad), rdf_scbd_quad(M, Node, Quad), Quads).



%! rdf_scbd_triple(?M, +Node, -Triple) is nondet.
%! rdf_scbd_triple(?M, +Node, ?G, -Triple) is nondet.
%! rdf_scbd_triples(?M, ?Node, -Triples) is det.
%! rdf_scbd_triples(?M, ?Node, ?G, -Triples) is det.

rdf_scbd_triple(M, Node, Triple) :-
  rdf_scbd_triple(M, Node, _, Triple).


rdf_scbd_triple(M, Node, G, Triple) :-
  rdf_scbd_triple0(M, Node, [Node], G, Triple).


rdf_scbd_triple0(M, O, Hist, G, Triple) :-
  rdf_cbd_inv_triple0(M, O, Hist, G, Triple).
rdf_scbd_triple0(M, S, Hist, G, Triple) :-
  rdf_cbd_triple0(M, S, Hist, G, Triple).


rdf_cbd_inv_triple0(M, O, Hist1, G, Triple) :-
  t(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(S),
      node_history0(Hist1, S, Hist2),
      rdf_cbd_inv_triple0(M, S, Hist2, G, Triple)
  ;   rdf_reification(M, S, P, O, G, Stmt),
      node_history0(Hist1, Stmt, Hist2),
      rdf_scbd_triple0(M, Stmt, Hist2, G, Triple)
  ).


rdf_scbd_triples(M, Node, Triples) :-
  rdf_scbd_triples(M, Node, _, Triples).


rdf_scbd_triples(M, Node, G, Triples) :-
  rdf_term(M, Node, G),
  aggregate_all(set(Triple), rdf_scbd_triple(M, Node, G, Triple), Triples).



%! rdf_tree_triple(?M,  +Root,     -Triple ) is nondet.
%! rdf_tree_triple(?M,  +Root, ?G, -Triple ) is nondet.
%! rdf_tree_triples(?M, ?Root,     -Triples) is det.
%! rdf_tree_triples(?M, ?Root, ?G, -Triples) is det.

rdf_tree_triple(M, Root, Triple) :-
  rdf_tree_triple(M, Root, _, Triple).


rdf_tree_triple(M, Root, G, Triple) :-
  rdf_tree_triple0(M, Root, [Root], G, Triple).


rdf_tree_triple0(M, S, Hist1, G, Triple) :-
  rdf_is_subject(S),
  t(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_subject(O),
      node_history0(Hist1, O, Hist2),
      rdf_tree_triple0(M, O, Hist2, G, Triple)
  ).



%! rdf_tree_triples(+M, +Root, -Triples) is det.
%! rdf_tree_triples(+M, +Root, ?G, -Triples) is det.

rdf_tree_triples(M, Root, Triples) :-
  rdf_tree_triples(M, Root, _, Triples).


rdf_tree_triples(M, Root, G, Triples) :-
  rdf_subject(M, Root, G),
  aggregate_all(set(Triple), rdf_tree_triple(M, Root, Triple), Triples).





% HELPERS %

node_history0(Hist, O, _) :-
  memberchk(O, Hist), !,
  fail.
node_history0(Hist1, O, Hist2) :-
  ord_add_element(Hist1, O, Hist2).
