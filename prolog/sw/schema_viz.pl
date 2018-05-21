:- module(
  schema_viz,
  [
    export_hierarchy/3, % +File, +P, +G
    export_hierarchy/4, % +File, +P, +G, +Options
    view_hierarchy/2,   % +P, +G
    view_hierarchy/3    % +P, +G, +Options
  ]
).

/** <module> Schema visualization

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(debug_ext)).
:- use_module(library(graph/graph_export)).
:- use_module(library(graph/graph_ext)).
:- use_module(library(os_ext)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_print)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(uri_ext)).

:- rdf_meta
   export_hierarchy(+, r, r),
   export_hierarchy(+, r, r, +),
   view_hierarchy(r, r),
   view_hierarchy(r, r, +).





%! export_arc(+Out:stream, +Arc:compound, +Options:list(compound)) is det.

export_arc(Out, arc(C1,[P],C2), _) :-
  rdf_prefix_memberchk(P, [rdfs:subClassOf,rdfs:subPropertyOf]), !,
  % By swapping the order in which the nodes are asserted, we are able
  % to show superclasses/superproperties above
  % subclasses/subproperties.
  dot_arc(Out, C2, C1, [arrowtail(onormal),dir(back),'URL'(P)]).
export_arc(Out, arc(C1,Ps,C2), Options) :-
  maplist({Options}/[P,Label]>>export_node_label(P, Label, Options), Ps, Labels),
  atomics_to_string(Labels, "/", Label),
  % We cannot put in URLs for all property path members, so we only
  % take the first one.
  Ps = [P|_],
  dot_arc(Out, C1, C2, [label(Label),'URL'(P)]).



%! export_hierarchy(+File:atom, +P:rdf_predicate, +G:rdf_graph) is det.
%! export_hierarchy(+File:atom, +P:rdf_predicate, +G:rdf_graph,
%!                  +Options:list(compound)) is det.

export_hierarchy(File, P, G) :-
  export_hierarchy(File, P, G, []).


export_hierarchy(File, P, G, Options) :-
  export_graph(
    File,
    {P,G,Options}/[Out]>>export_hierarchy_(Out, P, G, Options),
    Options
  ).

export_hierarchy_(Out, P, G, Options) :-
  aggregate_all(set(arc(C,[P],D)), rdf_triple(C, P, D, G), Arcs),
  maplist({Out,Options}/[Arc]>>export_arc(Out, Arc, Options), Arcs),
  arcs_to_vertices(Arcs, Nodes),
  maplist({Out,Options}/[Node]>>export_node(Out, Node, Options), Nodes).



%! export_node(+Out:stream, +Node:rdf_node, +Options:list(compound)) is det.
%
% Export a simple node, i.e., without its internal UML-like
% definition.

export_node(Out, Node, Options) :-
  export_node_label(Node, Label, Options),
  (is_http_uri(Node) -> T = ['URL'(Node)] ; T = []),
  dot_node(Out, Node, [label(Label),shape(rect)|T]).



%! export_node_label(+Node:rdf_node, -Label:string, +Options:list(compound)) is det.

export_node_label(Node, Label, Options) :-
  option(label_property(P0), Options),
  rdf_global_id(P0, P),
  rdf_triple(Node, P, Label, _), !.
export_node_label(Node, Label, _) :-
  string_phrase(rdf_dcg_term(Node), Label).



%! view_hierarchy(+P:rdf_predicate, +G:rdf_graph) is det.
%! view_hierarchy(+P:rdf_predicate, +G:rdf_graph, +Options:list(compound)) is det.

view_hierarchy(P, G) :-
  view_hierarchy(P, G, []).


view_hierarchy(P, G, Options) :-
  view_graph({P,G,Options}/[Out]>>export_hierarchy_(Out, P, G, Options), Options).
