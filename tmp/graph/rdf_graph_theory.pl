:- module(
  rdf_graph_theory,
  [
    rdf_graph_to_srep/3, % +Graph:atom
                         % -UGraph:ugraph
                         % +Options:list(compound)
    rdf_neighbor_vertex/4, % ?Graph:atom
                           % +Vertex
                           % -NeighborVertex
                           % +Options:list(compound)
    rdf_undirected_edge/3 % ?Graph:atom
                          % ?UndirectedEdge:compound
                          % +Options:list(compound)
  ]
).

/** <module> RDF graph theory

Graph theory support for RDF.

Graph theoretic insights cannot be directly applied to RDF graphs because
 edges (as defined by RDF abstract syntax) in one triple can be nodes in
 another.
This means that the definitions 'edge' and 'vertex' for graph theoretic
 operations of RDF data must be redefined.

@author Wouter Beek
@version 2012/01-2013/03, 2013/08, 2014/03, 2014/07, 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_term)).

:- predicate_options(rdf_graph_to_srep/3, 3, [
     pass_to(rdf_undirected_edge/3, 3),
     pass_to(rdf_vertex/3, 3)
   ]).
:- predicate_options(rdf_neighbor_vertex/4, 4, [
     pass_to(rdf_vertex_filter/2, 2)
   ]).
:- predicate_options(rdf_undirected_edge/3, 3, [
     pass_to(rdf_vertex_filter/2, 2)
   ]).
:- predicate_options(rdf_vertex/3, 3, [
     pass_to(rdf_vertex_filter/2, 2)
   ]).
:- predicate_options(rdf_vertex_filter/2, 2, [
     exclude_literals(+boolean),
     exclude_list_elements(+boolean)
   ]).





%! rdf_graph_to_srep(
%!   +Graph:atom,
%!   -UGraph:ugraph,
%!   +Options:list(compound)
%! ) is det.
% Returns the UGraph representation of a given RDF graph.
%
% Options are passed to rdf_vertex/3 and rdf_undirected_edge/3.

rdf_graph_to_srep(G, UG, Opts):-
  aggregate_all(
    set(V-Ws),
    (
      rdf_vertex(G, V, Opts),
      aggregate_all(set(W), rdf(V, _, W, G), Ws)
    ),
    UG
  ).



%! rdf_neighbor_vertex(
%!   ?Graph:atom,
%!   +Vertex:rdf_term,
%!   -Neighbor:rdf_term,
%!   +Options:list(compound)
%! ) is nondet.

rdf_neighbor_vertex(G, V, N, Opts):-
  rdf(V, _, N, G),
  rdf_vertex_filter(N, Opts).
rdf_neighbor_vertex(G, V, N, Opts):-
  rdf(N, _, V, G),
  rdf_vertex_filter(N, Opts).



%! rdf_undirected_edge(
%!   ?Graph:atom,
%!   ?UndirectedEdge:compound,
%!   +Options:list(nvpair)
%! ) is nondet.

rdf_undirected_edge(G, UndirectedE, Opts):-
  rdf(S, P, O, G),
  rdf_vertex_filter(S, Opts),
  rdf_vertex_filter(O, Opts),
  (   UndirectedE = edge(S,P,O)
  ;   UndirectedE = edge(O,P,S)
  ).



%! rdf_vertex(
%!   +Graph:atom,
%!   ?Vertex:rdf_term,
%!   +Options:list(compound)
%! ) is nondet.
% Pairs of graphs and nodes that occur in that graph.
% A node is either a subject or an object term in an
% RDF triple.
%
% The following options are supported:
%   * `exclude_list_elements(+boolean)`
%     Whether vertices that occur within some RDF list should be excluded
%     (`true`) or not (`false`, default).
%   * `exclude_literals(+boolean)`
%     Whether literals are excluded (`true`) or not (`false`, default).

rdf_vertex(G, V, Opts):-
  rdf_node2(V),
  rdf_term(V, G),
  rdf_vertex_filter(V, Opts).





% HELPERS %

%! rdf_vertex_filter(+Vertex:rdf_term, +Options:list(compound)) is semidet.

rdf_vertex_filter(V, Opts):-
  % Literal filtering.
  (   rdf_is_literal(V)
  ->  \+ option(exclude_literals(true), Opts)
  ;   true
  ),

  % RDF list filtering.
  (   option(exclude_list_elements(true), Opts)
  ->  % Does not belong to an RDF list.
      \+ rdf_has(_, rdf:first, V),
      \+ rdf_has(_, rdf:rest, V)
  ;   true
  ).
