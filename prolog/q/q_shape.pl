:- module(
  q_shape,
  [
    q_cbd_quad/3,     % ?M, +Node,     -Quad
    q_cbd_quads/3,    % ?M, +Node,     -Quads
    q_cbd_triple/3,   % ?M, +Node,     -Triple
    q_cbd_triple/4,   % ?M, +Node, ?G, -Triple
    q_cbd_triples/3,  % ?M, ?Node,     -Triples
    q_cbd_triples/4,  % ?M, ?Node, ?G, -Triples
    q_root/2,         % ?M, ?Root
    q_root/3,         % ?M, ?Root, ?G
    q_scbd_quad/3,    % ?M, +Node,     -Quad
    q_scbd_quads/3,   % ?M, +Node,     -Quads
    q_scbd_triple/3,  % ?M, +Node,     -Triple
    q_scbd_triple/4,  % ?M, +Node, ?G, -Triple
    q_scbd_triples/3, % ?M, ?Node,     -Triples
    q_scbd_triples/4, % ?M, ?Node, ?G, -Triples
    q_tree_triple/3,  % ?M, ?Root,     -Triple
    q_tree_triple/4,  % ?M, ?Root, ?G, -Triple
    q_tree_triples/3, % ?M, ?Root,     -Triples
    q_tree_triples/4  % ?M, ?Root, ?G, -Triples
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
@version 2016/06-2016/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   q_cbd_quad(?, o, -),
   q_cbd_quads(?, o, -),
   q_cbd_triple(?, o, -),
   q_cbd_triple(?, o, r, -),
   q_cbd_triples(?, o, -),
   q_cbd_triples(?, o, r, -),
   q_root(?, r),
   q_root(?, r, r),
   q_scbd_quad(?, o, -),
   q_scbd_quads(?, o, -),
   q_scbd_triple(?, o, -),
   q_scbd_triple(?, o, r, -),
   q_scbd_triples(?, o, -),
   q_scbd_triples(?, o, r, -),
   q_tree_triple(?, r, -),
   q_tree_triple(?, r, r, -),
   q_tree_triples(?, r, -),
   q_tree_triples(?, r, r, -).





%! q_cbd_quad(?M, +Node, -Quad) is nondet.
%! q_cbd_quad(?M, +Node, -Quad) is nondet.

q_cbd_quad(M, Node, rdf(S,P,O,G)) :-
  q_cbd_triple(M, Node, G, rdf(S,P,O)).


q_cbd_quads(M, Node, Quads) :-
  q_subject(M, Node),
  aggregate_all(set(Quad), q_cbd_quad(M, Node, Quad), Quads).



%! q_cbd_triple(?M, +Node, -Triple) is nondet.
%! q_cbd_triple(?M, +Node, ?G, -Triple) is nondet.
%! q_cbd_triples(?M, ?Node, -Triples) is det.
%! q_cbd_triples(?M, ?Node, ?G, -Triples) is det.

q_cbd_triple(M, Node, Triple) :-
  q_cbd_triple(M, Node, _, Triple).


q_cbd_triple(M, Node, G, Triple) :-
  q_cbd_triple0(M, Node, [Node], G, Triple).


q_cbd_triple0(M, S, Hist1, G, Triple) :-
  rdf_is_subject(S),
  q(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   q_is_bnode(O),
      node_history0(Hist1, O, Hist2),
      q_cbd_triple0(M, O, Hist2, G, Triple)
  ;   q_reification(M, S, P, O, Stmt),
      node_history0(Hist1, Stmt, Hist2),
      q_cbd_triple0(M, Stmt, Hist2, G, Triple)
  ).


q_cbd_triples(M, Node, Triples) :-
  q_cbd_triples(M, Node, _, Triples).


q_cbd_triples(M, Node, G, Triples) :-
  q_subject(M, Node, G),
  aggregate_all(set(Triple), q_cbd_triple(M, Node, G, Triple), Triples).



%! q_root(?M, ?Root) is nondet.
%! q_root(?M, ?Root, ?G) is nondet.

q_root(M, Root) :-
  q_root(M, Root, _).


q_root(M, Root, G) :-
  q_subject(M, Root, G),
  \+ q(M, _, _, Root, G).



%! q_scbd_quad(?M, +Node, -Quad) is nondet.
%! q_scbd_quads(?M, +Node, -Quads) is nondet.

q_scbd_quad(M, Node, rdf(S,P,O,G)) :-
  q_scbd_triple(M, Node, G, rdf(S,P,O)).


q_scbd_quads(M, Node, Quads) :-
  q_subject(M, Node),
  aggregate_all(set(Quad), q_scbd_quad(M, Node, Quad), Quads).



%! q_scbd_triple(?M, +Node, -Triple) is nondet.
%! q_scbd_triple(?M, +Node, ?G, -Triple) is nondet.
%! q_scbd_triples(?M, ?Node, -Triples) is det.
%! q_scbd_triples(?M, ?Node, ?G, -Triples) is det.

q_scbd_triple(M, Node, Triple) :-
  q_scbd_triple(M, Node, _, Triple).


q_scbd_triple(M, Node, G, Triple) :-
  q_scbd_triple0(M, Node, [Node], G, Triple).


q_scbd_triple0(M, O, Hist, G, Triple) :-
  q_cbd_inv_triple0(M, O, Hist, G, Triple).
q_scbd_triple0(M, S, Hist, G, Triple) :-
  q_cbd_triple0(M, S, Hist, G, Triple).


q_cbd_inv_triple0(M, O, Hist1, G, Triple) :-
  q(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   q_is_bnode(S),
      node_history0(Hist1, S, Hist2),
      q_cbd_inv_triple0(M, S, Hist2, G, Triple)
  ;   q_reification(M, S, P, O, G, Stmt),
      node_history0(Hist1, Stmt, Hist2),
      q_scbd_triple0(M, Stmt, Hist2, G, Triple)
  ).


q_scbd_triples(M, Node, Triples) :-
  q_scbd_triples(M, Node, _, Triples).


q_scbd_triples(M, Node, G, Triples) :-
  q_term(M, Node, G),
  aggregate_all(set(Triple), q_scbd_triple(M, Node, G, Triple), Triples).



%! q_tree_triple(?M,  +Root,     -Triple ) is nondet.
%! q_tree_triple(?M,  +Root, ?G, -Triple ) is nondet.
%! q_tree_triples(?M, ?Root,     -Triples) is det.
%! q_tree_triples(?M, ?Root, ?G, -Triples) is det.

q_tree_triple(M, Root, Triple) :-
  q_tree_triple(M, Root, _, Triple).


q_tree_triple(M, Root, G, Triple) :-
  q_tree_triple0(M, Root, [Root], G, Triple).


q_tree_triple0(M, S, Hist1, G, Triple) :-
  rdf_is_subject(S),
  q(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   q_is_subject(O),
      node_history0(Hist1, O, Hist2),
      q_tree_triple0(M, O, Hist2, G, Triple)
  ).



%! q_tree_triples(+M, +Root, -Triples) is det.
%! q_tree_triples(+M, +Root, ?G, -Triples) is det.

q_tree_triples(M, Root, Triples) :-
  q_tree_triples(M, Root, _, Triples).


q_tree_triples(M, Root, G, Triples) :-
  q_subject(M, Root, G),
  aggregate_all(set(Triple), q_tree_triple(M, Root, Triple), Triples).





% HELPERS %

node_history0(Hist, O, _) :-
  memberchk(O, Hist), !,
  fail.
node_history0(Hist1, O, Hist2) :-
  ord_add_element(Hist1, O, Hist2).
