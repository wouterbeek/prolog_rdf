:- module(
  z_cbd,
  [
    z_cbd/2,         % ?Node,     -Triples
    z_cbd/3,         % ?Node, ?G, -Triples
    z_cbd_triple/2,  % +Node,     -Triple
    z_cbd_triple/3,  % +Node, ?G, -Triple
    z_scbd/2,        % ?Node,     -Triples
    z_scbd/3,        % ?Node, ?G, -Triples
    z_scbd_triple/2, % +Node,     -Triple
    z_scbd_triple/3  % +Node, ?G, -Triple
  ]
).

/** <module> Z (Symmmetric) Concise Bounded Description ((S)CBD)

# Concise Bounded Description (CBD)

  1. Include in the subgraph all statements in the source graph where
  the subject of the statement is the starting node.

  2. Recursively, for all statements identified in the subgraph thus
  far having a blank node object, include in the subgraph all
  statements in the source graph where the subject of the statement is
  the blank node in question and which are not already included in the
  subgraph.

  3. Recursively, for all statements included in the subgraph thus
  far, for all reifications of each statement in the source graph,
  include the concise bounded description beginning from the
  `rdf:Statement' node of each reification.

This results in a subgraph where the object nodes are either URI
references, literals, or blank nodes not serving as the subject of any
statement in the graph.


# Symmetric Concise Bounded Description (SCBD)

  1. Include in the subgraph all statements in the source graph where
  the object of the statement is the starting node;

  2. Recursively, for all statements identified in the subgraph thus
  far having a blank node subject not equal to the starting node,
  include in the subgraph all statements in the source graph where the
  object of the statement is the blank node in question and which are
  not already included in the subgraph.

  3. Recursively, for all statements included in the subgraph thus
  far, for all reifications of each statement in the source graph,
  include the symmetric concise bounded description beginning from the
  rdf:Statement node of each reification.

  4. Include in the subgraph the concise bounded description beginning
  from the starting node.

@author Wouter Beek
@version 2016/04, 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(z/z_stmt)).
:- use_module(library(z/z_term)).

:- rdf_meta
   z_cbd(o, -),
   z_cbd(o, r, -),
   z_cbd_triple(o, -),
   z_cbd_triple(o, r, -),
   z_scbd(o, -),
   z_scbd(o, r, -),
   z_scbd_triple(o, -),
   z_scbd_triple(o, r, -).





%! z_cbd(?Node, -Triples) is det.
%! z_cbd(?Node, ?G, -Triples) is det.
%! z_cbd_triple(+Node, -Triple) is nondet.
%! z_cbd_triple(+Node, ?G, -Triple) is nondet.

z_cbd(Node, Triples) :-
  z_cbd(Node, _, Triples).


z_cbd(Node, G, Triples) :-
  z_subject(Node, G),
  aggregate_all(set(Triple), z_cbd_triple0(Node, G, Triple), Triples).


z_cbd_triple(Node, Triple) :-
  z_cbd_triple(Node, _, Triple).


z_cbd_triple(Node, G, Triple) :-
  distinct(Triple, z_cbd_triple0(Node, G, Triple)).


z_cbd_triple0(S, G, Triple) :-
  z(S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(O),
      z_cbd_triple0(O, G, Triple)
  ;   z_reification(S, P, O, Stmt),
      z_cbd_triple0(Stmt, G, Triple)
  ).



%! z_scbd(?Node, -Triples) is det.
%! z_scbd(?Node, ?G, -Triples) is det.
%! z_scbd_triple(+Node, -Triple) is nondet.
%! z_scbd_triple(+Node, ?G, -Triple) is nondet.

z_scbd(Node, Triples) :-
  z_scbd(Node, _, Triples).


z_scbd(Node, G, Triples) :-
  z_term(Node, G),
  aggregate_all(set(Triple), z_scbd_triple0(Node, G, Triple), Triples).


z_scbd_triple(Node, Triple) :-
  z_scbd_triple(Node, _, Triple).


z_scbd_triple(Node, G, Triple) :-
  distinct(Triple, z_scbd_triple0(Node, G, Triple)).


z_scbd_triple0(O, G, Triple) :-
  z_cbd_inv_triple0(O, G, Triple).
z_scbd_triple0(S, G, Triple) :-
  z_cbd_triple0(S, G, Triple).


z_cbd_inv_triple0(O, G, Triple) :-
  z(S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(S),
      z_cbd_inv_triple0(S, G, Triple)
  ;   z_reification(S, P, O, G, Stmt),
      z_scbd_triple0(Stmt, G, Triple)
  ).
