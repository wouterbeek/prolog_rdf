:- module(
  fca_rdf,
  [
    fca_rdf
  ]
).

/** <module> Formal Concept Analysis: RDF

@author Wouter Beek
@version 2015/10
*/

fca_rdf(G):-
  rdf_graph(G), !,
  rdf(O, P, O0, G),
  A = popair(P,O0),
  

