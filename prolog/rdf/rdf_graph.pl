:- module(
  rdf_graph,
  [
    rdf_graph2/1, % @Term
    rdf_unload_graphs/0
  ]
).

/** <module> RDF graph

@author Wouter Beek
@compat [RDF 1.1 Semantics](http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/)
@license MIT
@version 2015/08
*/

:- use_module(library(semweb/rdf_db)).





%! rdf_graph2(@Term) is semidet.
% rdf_graph/1 throws an exception for any non-atomic nonvar argument,
% whereas this predicate fails silently.
%
% The name of this predicate is in line with rdf_is_bnode/1, rdf_is_literal/1,
%  and rdf_is_resource/1 in [library(semweb/rdf_db)].

rdf_graph2(G):-
  atom(G),
  rdf_graph(G).



%! rdf_unload_graphs is det.

rdf_unload_graphs:-
  rdf_graph(G),
  rdf_unload_graph(G),
  fail.
rdf_unload_graphs.
