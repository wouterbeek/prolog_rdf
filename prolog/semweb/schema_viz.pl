:- module(
  schema_viz,
  [
    export_hierarchy/3, % +File, +Backend, +P
    export_hierarchy/4, % +File, +Backend, +P, +Options
    view_hierarchy/2,   % +Backend, +P
    view_hierarchy/3    % +Backend, +P, +Options
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
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(uri_ext)).

:- rdf_meta
   export_hierarchy(+, t, r),
   export_hierarchy(+, t, r, +),
   view_hierarchy(t, r),
   view_hierarchy(t, r, +).





%! export_arc(+Out:stream, +Backend, +Arc:compound, +Options:list(compound)) is det.

export_arc(Out, B, arc(C1,[P],C2), _) :-
  rdf_prefix_memberchk(P, [rdfs:subClassOf,rdfs:subPropertyOf]), !,
  % By swapping the order in which the nodes are asserted, we are able
  % to show superclasses/superproperties above
  % subclasses/subproperties.
  dot_arc(Out, C2, C1, [arrowtail(onormal),dir(back),'URL'(P)]).
export_arc(Out, B, arc(C1,Ps,C2), Options) :-
  maplist({Options}/[P,Label]>>export_node_label(B, P, Label, Options), Ps, Labels),
  atomics_to_string(Labels, "/", Label),
  % We cannot put in URLs for all property path members, so we only
  % take the first one.
  Ps = [P|_],
  dot_arc(Out, C1, C2, [label(Label),'URL'(P)]).



%! export_hierarchy(+File:atom, +Backend, +P:rdf_predicate) is det.
%! export_hierarchy(+File:atom, +Backend, +P:rdf_predicate, +Options:list(compound)) is det.

export_hierarchy(File, B, P) :-
  export_hierarchy(File, B, P, []).


export_hierarchy(File, B, P, Options) :-
  export_graph(
    File,
    {B,P,Options}/[Out]>>export_hierarchy_(Out, B, P, Options),
    Options
  ).

export_hierarchy_(Out, B, P, Options) :-
  aggregate_all(set(arc(C,[P],D)), tp(B, C, P, D), Arcs),
  maplist({Out,B,Options}/[Arc]>>export_arc(Out, B, Arc, Options), Arcs),
  arcs_to_vertices(Arcs, Nodes),
  maplist({Out,B,Options}/[Node]>>export_node(Out, B, Node, Options), Nodes).



%! export_node(+Out:stream, +Backend, +Node:rdf_node, +Options:list(compound)) is det.
%
% Export a simple node, i.e., without its internal UML-like
% definition.

export_node(Out, B, Node, Options) :-
  export_node_label(B, Node, Label, Options),
  (is_http_uri(Node) -> T = ['URL'(Node)] ; T = []),
  dot_node(Out, Node, [label(Label),shape(rect)|T]).



%! export_node_label(+Backend, +Node:rdf_node, -Label:string, +Options:list(compound)) is det.

export_node_label(B, Node, Label, Options) :-
  option(label_property(P0), Options),
  rdf_prefix_iri(P0, P),
  tp(B, Node, P, Label), !.
export_node_label(_, Node, Label, _) :-
  string_phrase(rdf_dcg_node(Node), Label).



%! view_hierarchy(+Backend, +P:rdf_predicate) is det.
%! view_hierarchy(+Backend, +P:rdf_predicate, +Options:list(compound)) is det.

view_hierarchy(B, P) :-
  view_hierarchy(B, P, []).


view_hierarchy(B, P, Options) :-
  view_graph({B,P,Options}/[Out]>>export_hierarchy_(Out, B, P, Options), Options).
