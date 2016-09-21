:- module(
  q_graph_theory,
  [
    q_graph_edges/2, % ?G, -Es
    q_triple_edge/2  % ?Triple, ?E
  ]
).

/** <module> Quine graph theory

@author Wouter Beek
@version 2016/06, 2016/09
*/

:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   q_graph_edges(r, -).





%! q_graph_edges(?G, -Es) is det.
% Edges are the labeled edges in Graph.

q_graph_edges(G, Es) :-
  q_triples(G, Triples),
  maplist(q_triple_edge, Triples, Es).



%! q_triple_edge(+Triple, +E) is semidet.
%! q_triple_edge(+Triple, -E) is det.
%! q_triple_edge(-Triple, +E) is det.

q_triple_edge(rdf(S,P,O), edge(S,P,O)).
