:- module(
  z_graph_theory,
  [
    z_graph_edges/2, % ?G, -Es
    z_triple_edge/2  % ?Triple, ?E
  ]
).

/** <module> Z graph theory

@author Wouter Beek
@version 2015/12, 2016/03, 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(z/z_stmt)).

:- rdf_meta
   z_graph_edges(r, -).





%! z_graph_edges(?G, -Es) is det.
% Edges are the labeled edges in Graph.

z_graph_edges(G, Es) :-
  z_triples(G, Triples),
  maplist(z_triple_edge, Triples, Es).



%! z_triple_edge(+Triple, +E) is semidet.
%! z_triple_edge(+Triple, -E) is det.
%! z_triple_edge(-Triple, +E) is det.

z_triple_edge(rdf(S,P,O), edge(S,P,O)).
