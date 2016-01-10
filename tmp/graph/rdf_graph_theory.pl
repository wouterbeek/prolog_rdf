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

THIS MODULES HAS A FUNDAMENTAL PROBLEM: IT DOES NOT DEFINE WHAT HAPPENS
WITH PATHS THAT DO NOT EXIST IN THE NON-AGGREGATED GRAPH BUT THAT DO EXIST
IN THE AGGREGATED GRAPH.

      |
    --*--*--
         |

Graph theory support for RDF.

Graph theoretic insights cannot be directly applied to RDF graphs because
 edges (as defined by RDF abstract syntax) in one triple can be nodes in
 another.
This means that the definitions 'edge' and 'vertex' for graph theoretic
 operations of RDF data must be redefined.

@author Wouter Beek
@version 2012/01-2013/03, 2013/08, 2014/03, 2014/07, 2014/11, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(option)).

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
%!   +Graph:rdf_graph,
%!   -UGraph:ugraph,
%!   +Options:list(compound)
%! ) is det.
% Returns the UGraph representation of a given RDF graph.
%
% Options are passed to rdf_vertex/3 and rdf_undirected_edge/3.

rdf_graph_s_edges(G, UG, Opts) :-
  aggregate_all(
    set(V-Ws),
    (
      rdf_vertex(G, V, Opts),
      aggregate_all(set(W), rdf(V, _, W, G), Ws)
    ),
    UG
  ).



%! rdf_neighbor_vertex(
%!   ?Graph:rdf_graph,
%!   +Vertex:rdf_term,
%!   -Neighbor:rdf_term,
%!   +Options:list(compound)
%! ) is nondet.

rdf_neighbor_vertex(G, V, N, Opts) :-
  rdf(V, _, N, G),
  rdf_term_is_vertex(N, Opts).
rdf_neighbor_vertex(G, V, N, Opts) :-
  rdf(N, _, V, G),
  rdf_term_is_vertex(N, Opts).



%! rdf_undirected_edge(
%!   ?Graph:rdf_graph,
%!   ?UndirectedEdge:compound,
%!   +Options:list(compound)
%! ) is nondet.

rdf_undirected_edge(G, E, Opts) :-
  rdf(S, P, O, G),
  rdf_term_is_vertex(S, Opts),
  rdf_vertex_filter(O, Opts),
  (E = edge(S,P,O) ; E = edge(O,P,S)).



%! rdf_vertex(
%!   +Graph:rdf_graph,
%!   ?Vertex:rdf_term,
%!   +Options:list(compound)
%! ) is nondet.

rdf_vertex(G, V, Opts) :-
  rdf_node(G, V),
  rdf_term_is_vertex(V, Opts).



%! rdf_term_is_vertex(+Term:rdf_term, +Options:list(compound)) is semidet.
% Succeeds if the given Term is considered a vertex
% according to the given Options.
%
% The following options are supported:
%   * `exclude_list_elements(+boolean)`
%     Whether vertices that occur within some RDF list should be excluded
%     (`true`) or not (`false`, default).
%   * `exclude_literals(+boolean)`
%     Whether literals are excluded (`true`) or not (`false`, default).

rdf_term_is_vertex(V, Opts) :-
  (option(exclude_literals(true), Opts) -> \+ rdf_is_literal(V) ; true),
  (option(exclude_list_elements(true), Opts) -> \+ rdf_list(V) ; true).
