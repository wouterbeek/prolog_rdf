:- module(
  schema_viz,
  [
    export_hierarchy/4, % +Backend, +P, +Options, +Out
    show_hierarchy/3    % +Backend, +P, +Options
  ]
).
:- reexport(library(graph/gv)).

/** <module> Schema visualization

@author Wouter Beek
@version 2017/08-2018/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(graph/graph_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/t_api)).
:- use_module(library(uri/uri_ext)).

:- rdf_meta
   export_hierarchy(+, r, +, +),
   show_hierarchy(+, r, +).





%! export_edge(+Backend, +Options:dict, +Out:stream, +Edge:compound) is det.

export_edge(_, _, Out, edge(C1,[P],C2)) :-
  rdf_prefix_memberchk(P, [rdfs:subClassOf,rdfs:subPropertyOf]), !,
  maplist(gv_id, [C1,C2], [Id1,Id2]),
  % By swapping the order in which the nodes are asserted, we are able
  % to show superclasses/superproperties above
  % subclasses/subproperties.
  gv_edge(Out, Id2, Id1, [arrowtail(onormal),dir(back),'URL'(P)]).
export_edge(Backend, Options, Out, edge(C1,Ps,C2)) :-
  maplist(export_node_label(Backend, Options), Ps, PLabels),
  atomics_to_string(PLabels, "/", PsLabel),
  % We cannot put in URLs for all property path members, so we only
  % take the first one.
  Ps = [P|_],
  maplist(gv_id, [C1,C2], [Id1,Id2]),
  gv_edge(Out, Id1, Id2, [label(PsLabel),'URL'(P)]).



%! export_hierarchy(+Backend, +P:iri, +Options:dict, +Out:stream) is det.

export_hierarchy(Backend, P, Options, Out) :-
  format_debug(dot, Out, "digraph hierarchy {"),
  format_debug(dot, Out, "  graph [overlap=false];"),
  aggregate_all(set(edge(C,[P],D)), t(Backend, C, P, D), Edges),
  maplist(export_edge(Backend, Options, Out), Edges),
  edges_to_vertices(Edges, Nodes),
  maplist(export_node(Backend, Options, Out), Nodes),
  format_debug(dot, Out, "}").



%! export_node(+Backend, +Options:dict, +Out:stream, +Node:iri) is det.
%
% Export a simple node, i.e., without its internal UML-like
% definition.

export_node(Backend, Options, Out, Node) :-
  gv_id(Node, Id),
  export_node_label(Backend, Options, Node, Label),
  (is_http_uri(Node) -> T = ['URL'(Node)] ; T = []),
  gv_node(Out, Id, [label(Label),shape(rect)|T]).



%! export_node_label(+Backend, +Options:list(compound), +Node:rdf_term,
%!                   -Label:string) is det.

export_node_label(Backend, Options, Node, Label) :-
  _{label: P0} :< Options,
  rdf_prefix_iri(P0, P),
  t_label(Backend, Node, P, Label), !.
export_node_label(_, _, Node, Label) :-
  string_phrase(rdf_dcg_term(Node), Label).



%! show_hierarchy(+Backend, +P:iri, +Options:dict) is det.

show_hierarchy(Backend, P, Options) :-
  _{format: Ext, method: Method} :< Options,
  file_name_extension(hierarchy, Ext, File),
  gv_export(Method, Ext, File, export_hierarchy(Backend, P, Options)),
  open_file(File).
