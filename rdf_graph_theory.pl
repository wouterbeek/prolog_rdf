:- module(
  rdf_graph_theory,
  [
    rdf_directed_edge/2, % ?Graph:atom
                         % ?DirectedEdge:compound
    rdf_graph_to_ugraph/3, % +Graph:atom
                           % -UGraph:ugraph
                           % +Options:list(nvair)
    rdf_triples_to_edges/2, % +Triples:list(rdf_triple)
                            % -Edges:ordset(rdf_term)
    rdf_triples_to_vertices/2, % +Triples:list(rdf_triple)
                               % -Vertices:ordset(rdf_term)
    rdf_undirected_edge/2, % ?Graph:atom
                           % ?UndirectedEdge:compound
    rdf_vertex_equivalence/2 % +Resource1:uri
                             % +Resource2:uri
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
@version 2012/01-2013/03, 2013/08, 2014/03, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(predicate_options)). % Declaration.
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(rdf_list)).
:- use_module(plRdf(rdf_read)).
:- use_module(plRdf_term(rdf_language_tagged_string)).
:- use_module(plRdf_term(rdf_literal)).
:- use_module(plRdf_term(rdf_term)).

:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_vertex_equivalence(r,r)).

:- predicate_options(rdf_graph_to_ugraph/3, 3, [
     pass_to(rdf_vertex/3, 3)
   ]).
:- predicate_options(rdf_vertex/3, 3, [
     literal_filter(+boolean),
     rdf_list_filter(+boolean)
   ]).



%! rdf_directed_edge(+Graph:atom, +DirectedEdge:compound) is semidet.
%! rdf_directed_edge(+Graph:atom, -DirectedEdge:compound) is nondet.
%! rdf_directed_edge(-Graph:atom, +DirectedEdge:compound) is nondet.
%! rdf_directed_edge(-Graph:atom, -DirectedEdge:compound) is nondet.

rdf_directed_edge(Graph, FromV-P-ToV):-
  rdf(FromV, P, ToV, Graph).


%! rdf_graph_to_ugraph(
%!   +Graph:atom,
%!   -UGraph:compound,
%!   +Options:list(nvpair)
%! ) is det.
% Returns the UGraph representation of a given RDF graph.
%
% Options are passed to rdf_vertex/3.

rdf_graph_to_ugraph(Graph, UGraph, Options):-
  aggregate_all(
    set(From-Neighbors),
    (
      rdf_vertex(Graph, From, Options),
      aggregate_all(
        set(To),
        rdf_undirected_edge(Graph, From-_-To),
        Neighbors
      )
    ),
    UGraph
  ).


%! rdf_triples_to_edges(
%!   +Triples:list(or([bnode,iri,literal])),
%!   -Edges:ordset(compound)
%! ) is det.

rdf_triples_to_edges(Ts, Es):-
  aggregate_all(
    set(FromV-ToV),
    member(rdf(FromV,_,ToV), Ts),
    Es
  ).


%! rdf_triples_to_vertices(
%!   +Triples:list(or([bnode,iri,literal])),
%!   -Vertices:ordset(or([bnode,iri,literal]))
%! ) is det.

rdf_triples_to_vertices(Ts, Vs):-
  aggregate_all(
    set(V),
    (
      member(rdf(V1,_,V2), Ts),
      (
        V = V1
      ;
        V = V2
      )
    ),
    Vs
  ).


%! rdf_undirected_edge(+Graph:atom, +UndirectedEdge:compound) is semidet.
%! rdf_undirected_edge(+Graph:atom, -UndirectedEdge:compound) is nondet.
%! rdf_undirected_edge(-Graph:atom, +UndirectedEdge:compound) is nondet.
%! rdf_undirected_edge(-Graph:atom, -UndirectedEdge:compound) is nondet.

rdf_undirected_edge(Graph, FromV-P-ToV):-
  rdf(FromV, P, ToV, Graph).
rdf_undirected_edge(Graph, FromV-P-ToV):-
  rdf(ToV, P, FromV, Graph).


%! rdf_vertex(+Graph:atom, ?Vertex:rdf_term, +Options:list(nvpair)) is nondet.
% Pairs of graphs and nodes that occur in that graph.
% A node is either a subject or an object term in an
% RDF triple.
%
% The following options are supported:
%   1. =|literal_filter(+boolean)|=
%      Whether literals are considered vertices (`true`, default)
%      or not (`false`).
%   2. =|rdf_list_filter(+boolean)|=
%      Whether vertices that occur within some RDF list should be included
%      (`true`, default) or not (`false`).

rdf_vertex(Graph, Vertex, Options):-
  % Subject and object terms are vertices.
  (
    rdf_subject(Vertex, Graph)
  ;
    rdf_object(Vertex, Graph)
  ),
  
  % Literal filtering.
  (
    rdf_is_literal(Vertex)
  ->
    option(literal_filter(true), Options, true)
  ;
    true
  ),
  
  % RDF list filtering.
  (
    option(rdf_list_filter(true), Options, true)
  ->
    true
  ;
    \+ rdf_list_member(Vertex, _)
  ).


% @tbd What is this?
rdf_vertex_equivalence(X, Y):-
  % Subject
  forall(
    rdf_has(X, P, O),
    rdf_has(Y, P, O)
  ),
  forall(
    rdf_has(Y, P, O),
    rdf_has(X, P, O)
  ),
  % Predicate
  forall(
    rdf_has(S, X, O),
    rdf_has(S, Y, O)
  ),
  forall(
    rdf_has(S, Y, O),
    rdf_has(S, X, O)
  ),
  % Object
  forall(
    rdf_has(S, P, X),
    rdf_has(S, P, Y)
  ),
  forall(
    rdf_has(S, P, Y),
    rdf_has(S, P, X)
  ).

