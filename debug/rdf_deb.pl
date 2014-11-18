:- module(
  rdf_deb,
  [
    rdf_load_graph_deb/1, % +Graph:atom
    rdf_unload_graph_deb/1 % +Graph:atom
  ]
).

/** <module> RDF debug

Debug tools for working with RDF.

@author Wouter Beek
@version 2014/05
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(graph/rdf_graph)).



rdf_load_graph_deb(Graph):-
  var(Graph), !.
rdf_load_graph_deb(Graph):-
  rdf_statistics(triples_by_graph(Graph,GraphTriples)),
  rdf_statistics(triples(AllTriples)),
  debug(
    mem_triples,
    'PLUS ~:d triples (~:d total)',
    [GraphTriples,AllTriples]
  ).


%! rdf_unload_graph_deb(+Graph:atom) is det.

rdf_unload_graph_deb(Graph):-
  \+ rdf_is_graph(Graph), !.
rdf_unload_graph_deb(Graph):-
  rdf_statistics(triples_by_graph(Graph,GraphTriples)),
  rdf_unload_graph(Graph),
  rdf_statistics(triples(AllTriples)),
  debug(
    mem_triples,
    'MINUS ~:d triples (~:d total)',
    [GraphTriples,AllTriples]
  ).


