:- module(
  schema_viz,
  [
    export_class_edge/2,     % +Out, +Edge
    export_class_hierarchy/2 % +Out, +G
  ]
).
:- reexport(library(graph/gv)).

/** <module> Schema visualization

@author Wouter Beek
@version 2017/08-2017/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(graph/graph_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(uri/uri_ext)).

:- rdf_meta
   export_class_hierarchy(+, r).





%! export_class_edge(+Out:stream, +Edge:edge) is det.

export_class_edge(Out, edge(C1,[P],C2)) :-
  rdf_equal(rdfs:subClassOf, P), !,
  maplist(dot_id, [C1,C2], [Id1,Id2]),
  % By swapping the order in which the nodes are asserted, we are able
  % to show superclasses above subclasses.
  dot_edge(Out, Id2, Id1, [arrowtail(onormal),dir(back),'URL'(P)]).
export_class_edge(Out, edge(C1,Ps,C2)) :-
  property_path_label(Ps, Label),
  % We cannot put in URLs for all property path members, so we only
  % take the first one.
  Ps = [P|_],
  maplist(dot_id, [C1,C2], [Id1,Id2]),
  dot_edge(Out, Id1, Id2, [label(Label),'URL'(P)]).



%! export_class_hierarchy(+Out:stream, +G:iri) is det.

export_class_hierarchy(Out, G) :-
  format_debug(dot, Out, "digraph class_hierarchy {"),
  format_debug(dot, Out, "  graph [overlap=false];"),
  rdf_equal(P, rdfs:subClassOf),
  aggregate_all(set(edge(C,[P],D)), rdf(C, P, D, G), Edges),
  maplist(export_class_edge(Out), Edges),
  edges_to_vertices(Edges, Nodes),
  maplist(export_class_node(Out), Nodes),
  format_debug(dot, Out, "}").



%! export_class_node(+Out:stream, +Node:iri) is det.
%
% Export a simple node, i.e., without its internal UML-like
% definition.

export_class_node(Out, Node) :-
  dot_id(Node, Id),
  iri_label(Node, Label),
  (is_http_uri(Node) -> T = ['URL'(Node)] ; T = []),
  dot_node(Out, Id, [label(Label),shape(rect)|T]).
