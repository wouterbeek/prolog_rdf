:- module(
  rdf_cbd,
  [
    cbd/2,      % +Start, -Cbd
    cbd_size/2, % +Start, -N
    test_cbd/0
  ]
).

/** <module> RDF Concise Bounded Description (CBD)

  1. Include in the subgraph all statements in the source graph where the subject of the statement is the starting node.
  2. Recursively, for all statements identified in the subgraph thus far having a blank node object, include in the subgraph all statements in the source graph where the subject of the statement is the blank node in question and which are not already included in the subgraph.
  3. Recursively, for all statements included in the subgraph thus far, for all reifications of each statement in the source graph, include the concise bounded description beginning from the `rdf:Statement' node of each reification. 

This results in a subgraph where the object nodes are either URI references, literals, or blank nodes not serving as the subject of any statement in the graph. [WHAT?!]

---

@author Wouter Beek
@version 2015/04
*/

:- use_module(library(aggregate)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_isomorphism)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf11)).





test_cbd :-
  rdf_reset_db,
  
  PrintOpts = _{bnode_counter: 0},
  
  absolute_file_name(library('rdf/cbd1.rdf'), File1, [access(read)]),
  rdf_load_file(File1, [graph(cbd1)]),
  msg_notification("Full graph:~n"),
  rdf_print_graph(cbd1, PrintOpts),

  cbd('http://example.com/aReallyGreatBook', Cbd),
  msg_notification("Computed CBD:~n"),
  rdf_print_triples(Cbd, PrintOpts),
  
  absolute_file_name(library('rdf/cbd2.rdf'), File2, [access(read)]),
  rdf_load_file(File2, [graph(cbd2)]),
  msg_notification("Reference CBD:~n"),
  rdf_print_graph(cbd2, PrintOpts),

  rdf_isomorphic_graphs(Cbd, cbd2).



%! cbd(+Start, -Triples) is det.

cbd(Iri, Triples) :-
  aggregate_all(set(Triple), cbd_triple0(Iri, Triple), Triples).


cbd_triple0(S, Triple) :-
  rdf(S, P, O),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(O),
      cbd_triple0(O, Triple)
  ;   rdf_reification(S, P, O, Stmt),
      cbd_triple0(Stmt, Triple)
  ).



%! cbd(+Start, -N) is det.

cbd_size(State, N) :-
  cbd(State, Triples),
  length(Triples, N).
