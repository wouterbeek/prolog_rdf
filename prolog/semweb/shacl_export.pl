:- module(
  shacl_export,
  [
    export_shacl/2, % +File, +G
    export_shacl/3, % +File, +G, +Options
    view_shacl/1,   % +G
    view_shacl/2    % +G, +Options
  ]
).

/** <module> SHACL export

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
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(semweb/shacl)).

:- maplist(rdf_assert_prefix, [owl,rdf,sh]).

:- rdf_meta
   export_shacl(+, r),
   export_shacl(+, r, +),
   view_shacl(r),
   view_shacl(r, +).





%! export_shacl(+File:atom, +G:rdf_graph) is det.
%! export_shacl(+File:atom, +G:rdf_graph, +Options:list(compound)) is det.

export_shacl(File, G) :-
  export_shacl(File, G, []).


export_shacl(File, G, Options1) :-
  merge_options([directed(true)], Options1, Options2),
  export_graph(File, {G}/[Out]>>shacl_export_graph(Out, G), Options2).



%! view_shacl(+G:rdf_graph) is det.
%! view_shacl(+G:rdf_graph, +Options:list(compound)) is det.

view_shacl(G) :-
  view_shacl(G, []).


view_shacl(G, Options1) :-
  merge_options([directed(true)], Options1, Options2),
  view_graph(shacl_export_graph(G), Options2).





% GENERICS %

%! shacl_export_arc(+Out:stream, +Arc:compound) is det.

shacl_export_arc(Out, arc(C,[P],D)) :-
  rdf_prefix_memberchk(P, [rdfs:subClassOf,rdfs:subPropertyOf]), !,
  dot_arc(Out, D, C, [arrowtail(onormal),dir(back),'URL'(P)]).
shacl_export_arc(Out, arc(C,Ps,D)) :-
  maplist(shacl_node_label, Ps, PLabels),
  atomics_to_string(PLabels, "/", PsLabel),
  dot_arc(Out, C, D, [label(PsLabel)]).



%! shacl_export_class(+Out:stream, +C:atom, +G:atom) is det.

% The instances of class `C' form an OWL value list.
shacl_export_class(Out, C, G) :-
  aggregate_all(set(Value), rdf_triple_list_member(C, owl:oneOf, Value, G), Values),
  Values \== [], !,
  dot_id(C, CId),
  format_debug(dot, Out, "  ~a [label=<<TABLE>", [CId]),
  shacl_node_label(C, Label),
  format_debug(dot, Out, "    <TR><TD><B>~s</B></TD></TR>", [Label]),
  maplist(shacl_export_class_value(Out), Values),
  format_debug(dot, Out, '  </TABLE>>,shape="none",URL="~a"];', [C]).
% The instances of class `C' are related to certain property/range
% pairs.
shacl_export_class(Out, C, G) :-
  dot_id(C, CId),
  format_debug(dot, Out, "  ~a [label=<<TABLE>", [CId]),
  shacl_node_label(C, CLabel),
  format_debug(dot, Out, '    <TR><TD COLSPAN="2"><B>~s</B></TD></TR>', [CLabel]),
  aggregate_all(set(Pair), shacl_property_path_datatype(C, Pair, G), Pairs),
  maplist(shacl_export_class_path(Out), Pairs),
  format_debug(dot, Out, '  </TABLE>>,shape="none",URL="~a"];', [C]).



%! shacl_export_class_path(+Out:stream, +Pair:pair(list(rdf_predicate),rdf_object)) is det.

shacl_export_class_path(Out, Ps-O) :-
  maplist(shacl_node_label, [O|Ps], [OLabel|PLabels]),
  atomics_to_string(PLabels, "/", PLabel),
  format_debug(dot, Out, "    <TR><TD>~s</TD><TD>~s</TD></TR>", [PLabel,OLabel]).



%! shacl_export_class_value(+Out:stream, +Value:term) is det.

shacl_export_class_value(Out, Value) :-
  shacl_node_label(Value, Label),
  format_debug(dot, Out, "    <TR><TD>~s</TD></TR>", [Label]).



%! shacl_export_graph(+Out:stream, +G:rdf_graph) is det.

shacl_export_graph(Out, G) :-
  % Nodes can be described in multiple vocabularies: OWL, SHACL,
  % RDF(S).  We therefore first group all nodes with some description,
  % and then generate nodes for each one in sequence.
  aggregate_all(set(C), shacl_class(C, G), Cs),
  maplist({Out,G}/[C]>>shacl_export_class(Out, C, G), Cs),
  aggregate_all(set(Arc), shacl_arc(Arc, G), Arcs),
  maplist(shacl_export_arc(Out), Arcs).



%! shacl_node_label(+Term:rdf_term, -Label:string) is det.

shacl_node_label(Term, Label) :-
  string_phrase(rdf_dcg_term(Term), Label0),
  dot_html_replace(Label0, Label).
