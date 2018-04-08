:- module(
  rdf_http,
  [
    rdf_http_graph_query/2 % ?G, -Query
  ]
).

/** <module> HTTP support for RDF

@author Wouter Beek
@version 2018
*/

:- use_module(library(sw/rdf_term)).





%! rdf_http_graph_query(+G:rdf_graph, -Query:list(compound)) is det.

rdf_http_graph_query(G, []) :-
  var(G), !.
rdf_http_graph_query(G, []) :-
  rdf_default_graph(G), !.
rdf_http_graph_query(G, [g(GAtom)]) :-
  rdf_term_to_atom(G, GAtom).
