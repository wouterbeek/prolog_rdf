:- module(
  rdf_cbd,
  [
    cbd/2,        % ?Node, -Triples
    cbd_triple/2, % +Node, -Triple
    scbd/2,       % ?Node, -Triples
    scbd_triple/2 % +Node, -Triple
  ]
).

/** <module> RDF Concise Bounded Description (CBD)

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
@version 2015/04, 2015/06
*/

:- use_module(library(aggregate)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).





%! cbd(?Node, -Triples) is det.
%! cbd_triple(+Node, -Triple) is nondet.

cbd(Node, Triples) :-
  rdf_subject(Node),
  aggregate_all(set(Triple), cbd_triple0(Node, Triple), Triples).

cbd_triple(Node, Triple) :-
  distinct(Triple, cbd_triple0(Node, Triple)).

cbd_triple0(S, Triple) :-
  rdf(S, P, O),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(O),
      cbd_triple0(O, Triple)
  ;   rdf_reification(S, P, O, Stmt),
      cbd_triple0(Stmt, Triple)
  ).



%! scbd(?Node, -Triples) is det.
%! scbd_triple(+Node, -Triple) is nondet.

scbd(Node, Triples) :-
  rdf_subject(Node),
  aggregate_all(set(Triple), scbd_triple0(Node, Triple), Triples).


scbd_triple(Node, Triple) :-
  distinct(Triple, scbd_triple0(Node, Triple)).

scbd_triple0(O, Triple) :-
  rdf(S, P, O),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(S),
      scbd_triple0(S, Triple)
  ;   rdf_reification(S, P, O, Stmt),
      scbd_triple0(Stmt, Triple)
  ).
scbd_triple0(S, Triple) :-
  cbd_triple0(S, Triple).
