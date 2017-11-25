:- module(
  schema_viz,
  [
    class_hierarchy/2, % +Out, +G
    shacl_viz/2        % +Out, +G
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
:- use_module(library(uri/uri_ext)).
:- use_module(library(yall)).

:- rdf_meta
   edge(t, r),
   export_edge(+, t),
   shacle_viz(+, r).





%! class_hierarchy(+Out:stream, +G:iri) is det.

class_hierarchy(Out, G) :-
  format_debug(dot, Out, "digraph class_hierarchy {"),
  rdf_equal(P, rdfs:subClassOf),
  aggregate_all(set(edge(C,[P],D)), rdf(C, P, D, G), Edges),
  maplist(export_edge(Out), Edges),
  edges_to_vertices(Edges, Nodes),
  maplist(export_node(Out), Nodes),
  format_debug(dot, Out, "}").



%! shacl_viz(+Out:stream, +G:iri) is det.

shacl_viz(Out, G) :-
  % Nodes can be described in multiple vocabularies: OWL, SHACL,
  % RDF(S).  We therefore first group all nodes with some description,
  % and then generate nodes for each one in sequence.
  format_debug(dot, Out, "digraph shacl {"),
  aggregate_all(set(C), class(C, G), Cs),
  maplist({Out,G}/[C]>>export_class(Out, C, G), Cs),
  aggregate_all(set(Edge), edge(Edge, G), Edges),
  maplist(export_edge(Out), Edges),
  format_debug(dot, Out, "}").





% QUERY %

%! class(-C:atom, +G:atom) is nondet.
%
% Enumerates classes.

class(C, G) :-
  rdf(C, owl:oneOf, _, G).
%class(C, G) :-
%  rdf(_, rdf:type, C, G).
class(C, G) :-
  rdf(C, rdfs:subClassOf, _, G).
class(C, G) :-
  rdf(_, rdfs:subClassOf, C, G).
class(C, G) :-
  shape_class(C, G).



%! class_values(+C:atom, -Values:list(atom), +G:atom) is det.

class_values(C, Values, G) :-
  aggregate_all(
    set(Value),
    rdf_triple_list_member(C, owl:oneOf, Value, G),
    Values
  ),
  Values \== [].



%! edge(-Edge:compound, +G:atom) is det.
%
% Enumerates edges that describe schema and shapes.

edge(edge(C1,[rdfs:subClassOf],C2), G) :-
  rdf(C1, rdfs:subClassOf, C2, G).
edge(edge(C1,PP,C2), G) :-
  rdf(Shape, sh:targetClass, C1, G),
  rdf(Shape, sh:property, P, G),
  rdf(P, sh:path, Path, G),
  path_pp(Path, PP, G),
  rdf(P, sh:class, C2, G).



%! path_pp(+Path:atom, -PP:list(atom), +G:atom) is det.
%
% A property path is either composed on one segment or of a sequence
% (`rdf:List').

path_pp(Path, PP, G) :-
  aggregate_all(
    set(P),
    rdf_list_member(P, Path, G),
    PP
  ),
  PP \== [], !.
path_pp(P, [P], _).



%! pp(+C:atom, -Pair:pair(list(atom),atom), +G:atom) is nondet.
%
% Enumerates property path-target pairs for the given class `C'.

pp(C, PP-Target, G) :-
  rdf(Shape, sh:targetClass, C, G),
  rdf(Shape, sh:property, Property, G),
  rdf(Property, sh:path, Path, G),
  path_pp(Path, PP, G),
  target(Property, Target, G).



%! shape_class(+C:atom, +G:atom) is semidet.
%! shape_class(-C:atom, +G:atom) is nondet.

shape_class(C, G) :-
  rdf(Shape, rdf:type, sh:'NodeShape', G),
  rdf(Shape, sh:targetClass, C, G).



%! target(+P:atom, -Target:atom, +G:atom) is det.
%
% Target is the shape range for P.

target(P, D, G) :-
  rdf(P, sh:datatype, D, G).
target(P, C, G) :-
  rdf(P, sh:class, C, G).





% LABELS %

%! iri_label(+Iri:atom, -Label:string) is det.

iri_label(Iri, Label) :-
  rdf_prefix_iri(Prefix:Local, Iri), !,
  atomics_to_string([Prefix,Local], ":", Label).
iri_label(Iri, Label) :-
  atom_string(Iri, Label).



%! pp_label(+PP:list(atom), -PP_Label:string) is det.

pp_label(PP, PP_Label) :-
  maplist(iri_label, PP, PP_Labels),
  atomics_to_string(PP_Labels, /, PP_Label).





% EXPORT %

%! export_class(+Out:stream, +C:atom, +G:atom) is det.

export_class(Out, C, G) :-
  class_values(C, Values, G), !,
  dot_id(C, CId),
  format_debug(dot, Out, "  ~a [label=<<TABLE>", [CId]),
  iri_label(C, CLabel),
  format_debug(dot, Out, "    <TR><TD><B>~a</B></TD></TR>", [CLabel]),
  maplist(export_value(Out), Values),
  format_debug(dot, Out, '  </TABLE>>,shape="none",URL="~a"];', [C]).
export_class(Out, C, G) :-
  export_class_top(Out, C),
  aggregate_all(set(PP_Target), pp(C, PP_Target, G), PP_Targets),
  maplist(export_class_pp(Out), PP_Targets),
  export_class_bottom(Out, C).



%! export_class_bottom(+Out:stream, +C:atom) is det.

export_class_bottom(Out, C) :-
  format_debug(dot, Out, '  </TABLE>>,shape="none",URL="~a"];', [C]).



%! export_class_pp(+Out:stream, +PP_Target:pair(list(atom),atom)) is det.

export_class_pp(Out, PP-Target) :-
  pp_label(PP, PP_Label),
  iri_label(Target, TargetLabel),
  format_debug(
    dot,
    Out,
    "    <TR><TD>~a</TD><TD>~a</TD></TR>",
    [PP_Label,TargetLabel]
  ).



%! export_class_top(+Out:stream, +C:atom) is det.

export_class_top(Out, C) :-
  dot_id(C, CId),
  format_debug(dot, Out, "  ~a [label=<<TABLE>", [CId]),
  iri_label(C, CLabel),
  format_debug(
    dot,
    Out,
    '    <TR><TD COLSPAN="2"><B>~a</B></TD></TR>',
    [CLabel]
  ).



%! export_edge(+Out:stream, +Edge:compound) is det.

export_edge(Out, edge(C1,[P],C2)) :-
  rdf_equal(rdfs:subClassOf, P), !,
  maplist(dot_id, [C1,C2], [Id1,Id2]),
  % By swapping the order in which the nodes are asserted, we are able
  % to show superclasses above subclasses.
  dot_edge(Out, Id2, Id1, [arrowtail(onormal),dir(back),'URL'(P)]).
export_edge(Out, edge(C1,PP,C2)) :-
  pp_label(PP, Label),
  % We cannot put in URLs for all property path members, so we only
  % take the first one.
  PP = [P|_],
  maplist(dot_id, [C1,C2], [Id1,Id2]),
  dot_edge(Out, Id1, Id2, [label(Label),'URL'(P)]).



%! export_node(+Out:stream, +Node:iri) is det.
%
% Export a simple node, i.e., without its internal UML-like
% definition.

export_node(Out, Node) :-
  dot_id(Node, Id),
  iri_label(Node, Label),
  (is_http_uri(Node) -> T = ['URL'(Node)] ; T = []),
  dot_node(Out, Id, [label(Label),shape(rect)|T]).



%! export_value(+Out:stream, +Value:atom) is det.

export_value(Out, Value) :-
  iri_label(Value, ValueLabel),
  format_debug(dot, Out, "    <TR><TD>~a</TD></TR>", [ValueLabel]).
