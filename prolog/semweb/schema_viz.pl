:- module(
  schema_viz,
  [
    export_class_hierarchy/2,   % +Backend, +Out
    export_property_hierarchy/2 % +Backend, +Out
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
   export_hierarchy_(+, +, r).





%! export_class_hierarchy(+Backend, +Out:stream) is det.

export_class_hierarchy(Backend, Out) :-
  export_hierarchy_(Backend, Out, rdfs:subClassOf).



%! export_edge(+Backend, +Out:stream, +Edge:compound) is det.

export_edge(_, Out, edge(C1,[P],C2)) :-
  rdf_prefix_memberchk(P, [rdfs:subClassOf,rdfs:subPropertyOf]), !,
  maplist(dot_id, [C1,C2], [Id1,Id2]),
  % By swapping the order in which the nodes are asserted, we are able
  % to show superclasses/superproperties above
  % subclasses/subproperties.
  dot_edge(Out, Id2, Id1, [arrowtail(onormal),dir(back),'URL'(P)]).
export_edge(Backend, Out, edge(C1,Ps,C2)) :-
  rdf_property_path_label(Backend, Ps, Label),
  % We cannot put in URLs for all property path members, so we only
  % take the first one.
  Ps = [P|_],
  maplist(dot_id, [C1,C2], [Id1,Id2]),
  dot_edge(Out, Id1, Id2, [label(Label),'URL'(P)]).



%! export_node(+Backend, +Out:stream, +Node:iri) is det.
%
% Export a simple node, i.e., without its internal UML-like
% definition.

export_node(Backend, Out, Node) :-
  dot_id(Node, Id),
  rdf_term_label(Backend, Node, Label),
  (is_http_uri(Node) -> T = ['URL'(Node)] ; T = []),
  dot_node(Out, Id, [label(Label),shape(rect)|T]).



%! export_property_hierarchy(+Backend, +Out:stream) is det.

export_property_hierarchy(Backend, Out) :-
  export_hierarchy_(Backend, Out, rdfs:subPropertyOf).





% GENERICS %

%! export_hierarchy_(+Backend, +Out:stream, +P:iri) is det.

export_hierarchy_(Backend, Out, P) :-
  format_debug(dot, Out, "digraph hierarchy {"),
  format_debug(dot, Out, "  graph [overlap=false];"),
  aggregate_all(set(edge(C,[P],D)), t(Backend, C, P, D), Edges),
  maplist(export_edge(Backend, Out), Edges),
  edges_to_vertices(Edges, Nodes),
  maplist(export_node(Backend, Out), Nodes),
  format_debug(dot, Out, "}").
