:- module(
  rdf_cbd,
  [
    rdf_cbd/2,         % ?Node,     -Triples
    rdf_cbd/3,         % ?Node, ?G, -Triples
    rdf_cbd_triple/2,  % +Node,     -Triple
    rdf_cbd_triple/3,  % +Node, ?G, -Triple
    rdf_scbd/2,        % ?Node,     -Triples
    rdf_scbd/3,        % ?Node, ?G, -Triples
    rdf_scbd_triple/2, % +Node,     -Triple
    rdf_scbd_triple/3  % +Node, ?G, -Triple
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
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   rdf_cbd(o, -),
   rdf_cbd(o, r, -),
   rdf_cbd_triple(o, -),
   rdf_cbd_triple(o, r, -),
   rdf_scbd(o, -),
   rdf_scbd(o, r, -),
   rdf_scbd_triple(o, -),
   rdf_scbd_triple(o, r, -).





%! rdf_cbd(?Node, -Triples) is det.
%! rdf_cbd(?Node, ?G, -Triples) is det.
%! rdf_cbd_triple(+Node, -Triple) is nondet.
%! rdf_cbd_triple(+Node, ?G, -Triple) is nondet.

rdf_cbd(Node, Triples) :-
  rdf_cbd(Node, _, Triples).


rdf_cbd(Node, G, Triples) :-
  rdf_subject(Node, G),
  aggregate_all(set(Triple), rdf_cbd_triple0(Node, G, Triple), Triples).


rdf_cbd_triple(Node, Triple) :-
  rdf_cbd_triple(Node, _, Triple).


rdf_cbd_triple(Node, G, Triple) :-
  distinct(Triple, rdf_cbd_triple0(Node, G, Triple)).

rdf_cbd_triple0(S, G, Triple) :-
  rdf(S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(O),
      rdf_cbd_triple0(O, G, Triple)
  ;   rdf_reification(S, P, O, Stmt),
      rdf_cbd_triple0(Stmt, G, Triple)
  ).



%! rdf_scbd(?Node, -Triples) is det.
%! rdf_scbd(?Node, ?G, -Triples) is det.
%! rdf_scbd_triple(+Node, -Triple) is nondet.
%! rdf_scbd_triple(+Node, ?G, -Triple) is nondet.

rdf_scbd(Node, Triples) :-
  rdf_scbd(Node, _, Triples).


rdf_scbd(Node, G, Triples) :-
  rdf_term(Node, G),
  aggregate_all(set(Triple), rdf_scbd_triple0(Node, G, Triple), Triples).


rdf_scbd_triple(Node, Triple) :-
  rdf_scbd_triple(Node, _, Triple).


rdf_scbd_triple(Node, G, Triple) :-
  distinct(Triple, rdf_scbd_triple0(Node, G, Triple)).

rdf_scbd_triple0(O, G, Triple) :-
  rdf_cbd_inv_triple0(O, G, Triple).
rdf_scbd_triple0(S, G, Triple) :-
  rdf_cbd_triple0(S, G, Triple).

rdf_cbd_inv_triple0(O, G, Triple) :-
  rdf(S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(S),
      rdf_cbd_inv_triple0(S, G, Triple)
  ;   rdf_reification(S, P, O, G, Stmt),
      rdf_scbd_triple0(Stmt, G, Triple)
  ).
