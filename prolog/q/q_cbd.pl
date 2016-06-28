:- module(
  q_cbd,
  [
    q_cbd_triple/3,   % ?M, +Node,     -Triple
    q_cbd_triple/4,   % ?M, +Node, ?G, -Triple
    q_cbd_triples/3,  % ?M, ?Node,     -Triples
    q_cbd_triples/4,  % ?M, ?Node, ?G, -Triples
    q_scbd_triple/3,  % ?M, +Node,     -Triple
    q_scbd_triple/4,  % ?M, +Node, ?G, -Triple
    q_scbd_triples/3, % ?M, ?Node,     -Triples
    q_scbd_triples/4  % ?M, ?Node, ?G, -Triples
  ]
).

/** <module> Quine (Symmmetric) Concise Bounded Description ((S)CBD) API

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
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(aggregate)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   q_cbd_triple(?, o, -),
   q_cbd_triple(?, o, r, -),
   q_cbd_triples(?, o, -),
   q_cbd_triples(?, o, r, -),
   q_scbd_triple(?, o, -),
   q_scbd_triple(?, o, r, -),
   q_scbd_triples(?, o, -),
   q_scbd_triples(?, o, r, -).





%! q_cbd_triple(?M, +Node, -Triple) is nondet.
%! q_cbd_triple(?M, +Node, ?G, -Triple) is nondet.
%! q_cbd_triples(?M, ?Node, -Triples) is det.
%! q_cbd_triples(?M, ?Node, ?G, -Triples) is det.

q_cbd_triple(M, Node, Triple) :-
  q_cbd_triple(M, Node, _, Triple).


q_cbd_triple(M, Node, G, Triple) :-
  distinct(Triple, q_cbd_triple0(M, Node, G, Triple)).


q_cbd_triple0(M, S, G, Triple) :-
  q(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   q_is_bnode(O),
      q_cbd_triple0(M, O, G, Triple)
  ;   q_reification(M, S, P, O, Stmt),
      q_cbd_triple0(M, Stmt, G, Triple)
  ).


q_cbd_triples(M, Node, Triples) :-
  q_cbd_triples(M, Node, _, Triples).


q_cbd_triples(M, Node, G, Triples) :-
  q_subject(M, Node, G),
  aggregate_all(set(Triple), q_cbd_triple0(M, Node, G, Triple), Triples).



%! q_scbd_triple(?M, +Node, -Triple) is nondet.
%! q_scbd_triple(?M, +Node, ?G, -Triple) is nondet.
%! q_scbd_triples(?M, ?Node, -Triples) is det.
%! q_scbd_triples(?M, ?Node, ?G, -Triples) is det.

q_scbd_triple(M, Node, Triple) :-
  q_scbd_triple(M, Node, _, Triple).


q_scbd_triple(M, Node, G, Triple) :-
  distinct(Triple, q_scbd_triple0(M, Node, G, Triple)).


q_scbd_triple0(M, O, G, Triple) :-
  q_cbd_inv_triple0(M, O, G, Triple).
q_scbd_triple0(M, S, G, Triple) :-
  q_cbd_triple0(M, S, G, Triple).


q_cbd_inv_triple0(M, O, G, Triple) :-
  q(M, S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   q_is_bnode(S),
      q_cbd_inv_triple0(M, S, G, Triple)
  ;   q_reification(M, S, P, O, G, Stmt),
      q_scbd_triple0(M, Stmt, G, Triple)
  ).


q_scbd_triples(M, Node, Triples) :-
  q_scbd_triples(M, Node, _, Triples).


q_scbd_triples(M, Node, G, Triples) :-
  q_term(M, Node, G),
  aggregate_all(set(Triple), q_scbd_triple0(M, Node, G, Triple), Triples).
