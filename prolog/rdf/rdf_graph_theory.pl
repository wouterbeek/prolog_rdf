:- module(
  rdf_graph_theory,
  [
    rdf_graph_edges/2, % ?Graph:rdf_graph
                       % -Edges:ordset(compound)
    rdf_triple_edge/2 % ?Triple:rdf_triple
                      % ?Edge:compound
  ]
).

/** <module> RDF graph theory

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_statement)).

:- rdf_meta(rdf_graph_edges(r,-)).





%! rdf_graph_edges(?Graph:rdf_graph, -Edges:ordset(rdf_triple)) is det.
% Edges are the labeled edges in Graph.

rdf_graph_edges(G, Es):-
  rdf_graph_triples(G, Ts),
  maplist(rdf_triple_edge, Ts, Es).



%! rdf_triple_edge(+Triple:rdf_triple, +Edge:compound) is semidet.
%! rdf_triple_edge(+Triple:rdf_triple, -Edge:compound) is det.
%! rdf_triple_edge(-Triple:rdf_triple, +Edge:compound) is det.

rdf_triple_edge(rdf(S,P,O), edge(S,P,O)).
