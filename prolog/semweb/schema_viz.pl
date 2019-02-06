:- module(
  schema_viz,
  [
    export_hierarchy/5, % +File, +Backend, ?C, +P, +Options
    view_hierarchy/4    % +Backend, ?C, +P, +Options
  ]
).

/** <module> Schema visualization

@author Wouter Beek
@version 2017-2019
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(debug_ext)).
:- use_module(library(graph/dot)).
:- use_module(library(graph/graph_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(uri_parse)).

is_meta(label_goal).

:- rdf_meta
   export_hierarchy(+, t, o, r, :),
   view_hierarchy(t, o, r, :).





%! export_arc(+Out:stream, +Backend, +Arc:compound, +Options:list(compound)) is det.

export_arc(Out, _, arc(C1,[P],C2), _) :-
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



%! export_hierarchy(+File:atom, +Backend, ?C:rdf_term, +P:rdf_predicate, +Options:list(compound)) is det.

export_hierarchy(File, B, C, P, Options0) :-
  meta_options(is_meta, Options0, Options1),
  merge_options([directed(true)], Options1, Options2),
  gv_export(
    File,
    {B,C,P,Options1}/[Out]>>export_hierarchy_(Out, B, C, P, Options1),
    Options2
  ).

export_hierarchy_(Out, B, C, P, Options) :-
  collect_hierarchy_arcs(B, [C], P, []-Arcs),
  maplist({Out,B,Options}/[Arc]>>export_arc(Out, B, Arc, Options), Arcs),
  arcs_to_vertices(Arcs, Nodes),
  maplist({Out,B,Options}/[Node]>>export_node(Out, B, Node, Options), Nodes).

collect_hierarchy_arcs(B, Cs1, P, Arcs1-Arcs) :-
  aggregate_all(
    set(C2-arc(C2,[P],C1)),
    (
      member(C1, Cs1),
      tp(B, C2, P, C1)
    ),
    Pairs
  ),
  (   Pairs == []
  ->  Arcs = Arcs1
  ;   pairs_keys_values(Pairs, Cs2, Arcs2),
      ord_union(Arcs1, Arcs2, Arcs3),
      collect_hierarchy_arcs(B, Cs2, P, Arcs3-Arcs)
  ).

%! export_node(+Out:stream, +Backend, +Node:rdf_node, +Options:list(compound)) is det.
%
% Export a simple node, i.e., without its internal UML-like
% definition.

export_node(Out, B, Node, Options) :-
  export_node_label(B, Node, Label, Options),
  (is_http_uri(Node) -> T = ['URL'(Node)] ; T = []),
  dot_node(Out, Node, [label(Label),shape(rect)|T]).



%! export_node_label(+Backend, +Node:rdf_node, -Label:string, +Options:list(compound)) is det.
%
% @arg Options The following options are supported:
%
%   * label_goal(Goal_2)
%   * label_predicate(iri)

export_node_label(_, Node, Label, Options) :-
  option(label_goal(Goal_2), Options),
  call(Goal_2, Node, Label), !.
export_node_label(B, Node, Label, Options) :-
  option(label_predicate(P0), Options),
  rdf_prefix_iri(P0, P),
  tp(B, Node, P, Label), !.
export_node_label(_, Node, Label, _) :-
  string_phrase(rdf_dcg_node(Node), Label).



%! view_hierarchy(+Backend, ?C:rdf_term, +P:rdf_predicate, +Options:list(compound)) is det.

view_hierarchy(B, C, P, Options0) :-
  meta_options(is_meta, Options0, Options1),
  merge_options([directed(true)], Options1, Options2),
  gv_view(
    {B,C,P,Options1}/[Out]>>export_hierarchy_(Out, B, C, P, Options1),
    Options2
  ).
