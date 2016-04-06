:- module(
  rdf_graph_theory,
  [
    rdf_graph_edges/2, % ?G, -Es
    rdf_triple_edge/2  % ?Triple, ?E
  ]
).

/** <module> RDF graph theory

@author Wouter Beek
@version 2015/12, 2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_graph_edges(r, -).





%! rdf_graph_edges(?G, -Es) is det.
% Edges are the labeled edges in Graph.

rdf_graph_edges(G, Es) :-
  rdf_graph_to_triples(G, Triples),
  maplist(rdf_triple_edge, Triples, Es).



%! rdf_triple_edge(+Triple, +E) is semidet.
%! rdf_triple_edge(+Triple, -E) is det.
%! rdf_triple_edge(-Triple, +E) is det.

rdf_triple_edge(rdf(S,P,O), edge(S,P,O)).
