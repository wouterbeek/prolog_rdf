:- module(
  rdf_graph_theory,
  [
    rdf_directed_edge/3, % ?Graph:atom
                         % ?DirectedEdge:compound
                         % +Options:list(nvair)
    rdf_graph_to_srep/3, % +Graph:atom
                         % -UGraph:ugraph
                         % +Options:list(nvair)
    rdf_neighbor_vertex/4, % ?Graph:atom
                           % +Vertex
                           % -NeighborVertex
                           % +Options:list(nvpair)
    rdf_undirected_edge/3 % ?Graph:atom
                          % ?UndirectedEdge:compound
                          % +Options:list(nvair)
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
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(term/rdf_list)).
:- use_module(plRdf(term/rdf_term)).

:- predicate_options(rdf_directed_edge/3, 3, [
  pass_to(rdf_vertex_filter/2, 2)
]).
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





%! rdf_directed_edge(
%!   +Graph:atom,
%!   ?DirectedEdge:compound,
%!   +Options:list(nvpair)
%! ) is nondet.

rdf_directed_edge(Graph, edge(S,P,O), Options):-
  rdf(S, P, O, Graph),
  rdf_vertex_filter(S, Options),
  rdf_vertex_filter(O, Options).



%! rdf_graph_to_srep(
%!   +Graph:atom,
%!   -UGraph:ugraph,
%!   +Options:list(nvpair)
%! ) is det.
% Returns the UGraph representation of a given RDF graph.
%
% Options are passed to rdf_vertex/3 and rdf_undirected_edge/3.

rdf_graph_to_srep(Graph, UGraph, Options):-
  aggregate_all(
    set(V-Ws),
    (
      rdf_vertex(Graph, V, Options),
      aggregate_all(
        set(W),
        rdf_directed_edge(Graph, edge(V,_,W), Options),
        Ws
      )
    ),
    UGraph
  ).


%! rdf_neighbor_vertex(
%!   ?Graph:atom,
%!   +Vertex:rdf_term,
%!   -Neighbor:rdf_term,
%!   +Options:list(nvpair)
%! ) is nondet.

rdf_neighbor_vertex(Graph, V, N, Options):-
  rdf(V, _, N, Graph),
  rdf_vertex_filter(N, Options).
rdf_neighbor_vertex(Graph, V, N, Options):-
  rdf(N, _, V, Graph),
  rdf_vertex_filter(N, Options).



%! rdf_undirected_edge(
%!   ?Graph:atom,
%!   ?UndirectedEdge:compound,
%!   +Options:list(nvpair)
%! ) is nondet.

rdf_undirected_edge(Graph, UndirectedEdge, Options):-
  rdf(S, P, O, Graph),
  rdf_vertex_filter(S, Options),
  rdf_vertex_filter(O, Options),
  (   UndirectedEdge = edge(S,P,O)
  ;   UndirectedEdge = edge(O,P,S)
  ).



%! rdf_vertex(+Graph:atom, ?Vertex:rdf_term, +Options:list(nvpair)) is nondet.
% Pairs of graphs and nodes that occur in that graph.
% A node is either a subject or an object term in an
% RDF triple.
%
% The following options are supported:
%   1. `exclude_list_elements(+boolean)`
%      Whether vertices that occur within some RDF list should be excluded
%      (`true`) or not (`false`, default).
%   2. `exclude_literals(+boolean)`
%      Whether literals are excluded (`true`) or not (`false`, default).

rdf_vertex(Graph, Vertex, Options):-
  % Subject and object terms are vertices.
  (   rdf_subject(Vertex, Graph)
  ;   rdf_object(Vertex, Graph)
  ),
  rdf_vertex_filter(Vertex, Options).





% HELPERS

%! rdf_vertex_filter(+Vertex:rdf_term, +Options:list(nvpair)) is semidet.

rdf_vertex_filter(V, Options):-
  % Literal filtering.
  (   rdf_is_literal(V)
  ->  \+ option(exclude_literals(true), Options)
  ;   true
  ),

  % RDF list filtering.
  (   option(exclude_list_elements(true), Options)
  ->  % Does not belong to an RDF list.
      \+ rdf_has(_, rdf:first, V),
      \+ rdf_has(_, rdf:rest, V)
  ;   true
  ).

