:- module(
  rdf_graph,
  [
    rdf_is_graph/1 % @Term
  ]
).

/** <module> RDF graph

@author Wouter Beek
@compat [RDF 1.1 Semantics](http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/)
@license MIT
@version 2015/08
*/

:- use_module(library(semweb/rdf_db)).





%! rdf_is_graph(@Term) is semidet.
% rdf_graph/1 throws an exception for any non-atomic nonvar argument,
% whereas this predicate fails silently.
%
% rdf_graph/1 does not succeed for the default graph (called `user`)
% if it is empty whereas this predicate does.
%
% The name of this predicate is in line with rdf_is_bnode/1, rdf_is_literal/1,
% and rdf_is_resource/1 in [library(semweb/rdf_db)].

rdf_is_graph(G):-
  atom(G),
  (G == user ; rdf_graph(G)), !.
