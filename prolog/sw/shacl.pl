:- module(
  shacl,
  [
    shacl_assert_class/1, % +Entry
    shacl_assert_class/2, % +Entry, +G
    shacl_export/1,       % +Out
    shacl_export/2        % +Out, +G
  ]
).

/** <module> SHACL

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(debug_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_print)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(sw/schema_viz)).

:- rdf_assert_prefix(sh).

:- rdf_meta
   shacl_assert_class(+, r),
   shacl_export(+, r).





%! shacl_assert_class(+Entry:compound) is det.
%! shacl_assert_class(+Entry:compound, +G:iri) is det.

shacl_assert_class(Entry) :-
  rdf_default_graph(G),
  shacl_assert_class(Entry, G).


shacl_assert_class(C-Groups, G) :-
  atom_concat(C, 'Shape', Shape),
  rdf_assert_triple(Shape, rdf:type, sh:'NodeShape', G),
  rdf_assert_triple(Shape, sh:targetClass, C, G),
  rdf_assert_list(Shape, sh:ignoredProperties, [rdf:type], G),
  maplist(shacl_assert_property(C, G), Groups).

shacl_assert_property(C, G, P-Os) :-
  rdf_create_bnode(BNode),
  rdf_assert_triple(C, sh:property, BNode, G),
  rdf_assert_triple(BNode, sh:path, P, G),
  (   Os = [O]
  ->  rdf_assert_triple(BNode, sh:class, O, G)
  ;   maplist(shacl_assert_object(G), Os, BNodes),
      rdf_assert_list(BNode, sh:or, BNodes, G)
  ).

shacl_assert_object(G, O, BNode) :-
  rdf_create_bnode(BNode),
  rdf_assert_triple(BNode, sh:class, O, G).



%! shacl_export(+Out:stream) is det.
%! shacl_export(+Out:stream, +G:iri) is det.

shacl_export(Out) :-
  shacl_export(Out, _).


shacl_export(Out, G) :-
  % top of graph
  format_debug(gv, Out, "digraph shacl {"),
  format_debug(gv, Out, "  graph [overlap=false];"),
  % Nodes can be described in multiple vocabularies: OWL, SHACL,
  % RDF(S).  We therefore first group all nodes with some description,
  % and then generate nodes for each one in sequence.
  aggregate_all(set(C), shacl_class(C, G), Cs),
  maplist({Out,G}/[C]>>shacl_export_class(Out, C, G), Cs),
  aggregate_all(set(Edge), shacl_edge(Edge, G), Edges),
  maplist(shacl_export_edge(Out), Edges),
  % bottom of graph
  format_debug(gv, Out, "}").



%! shacl_export_class(+Out:stream, +C:atom, +G:atom) is det.

% The instances of class `C' form an OWL value list.
shacl_export_class(Out, C, G) :-
  aggregate_all(
    set(Value),
    rdf_triple_list_member(C, owl:oneOf, Value, G),
    Values
  ),
  Values \== [], !,
  gv_id(C, CId),
  % top of node
  format_debug(gv, Out, "  ~a [label=<<TABLE>", [CId]),
  shacl_node_label(C, CLabel),
  format_debug(gv, Out, "    <TR><TD><B>~s</B></TD></TR>", [CLabel]),
  % middle of node
  forall(
    member(Value, Values),
    (
      shacl_node_label(Value, ValueLabel),
      format_debug(gv, Out, "    <TR><TD>~s</TD></TR>", [ValueLabel])
    )
  ),
  % bottom of node
  format_debug(gv, Out, '  </TABLE>>,shape="none",URL="~a"];', [C]).
% The instances of class `C' are related to certain property/range
% pairs.
shacl_export_class(Out, C, G) :-
  gv_id(C, CId),
  % top of node
  format_debug(gv, Out, "  ~a [label=<<TABLE>", [CId]),
  shacl_node_label(C, CLabel),
  format_debug(
    dot,
    Out,
    '    <TR><TD COLSPAN="2"><B>~s</B></TD></TR>',
    [CLabel]
  ),
  % middle of node
  aggregate_all(set(Pair), shacl_property_path_datatype(C, Pair, G), Pairs),
  forall(
    member(Ps-O, Pairs),
    (
      maplist(shacl_node_label, Ps, PLabels),
      atomics_to_string(PLabels, "/", PsLabel),
      shacl_node_label(O, OLabel),
      format_debug(
        dot,
        Out,
        "    <TR><TD>~s</TD><TD>~s</TD></TR>",
        [PsLabel,OLabel]
      )
    )
  ),
  % bottom of node
  format_debug(gv, Out, '  </TABLE>>,shape="none",URL="~a"];', [C]).



%! shacl_export_edge(+Out:stream, +Edge:compound) is det.

shacl_export_edge(Out, edge(C,Ps,D)) :-
  maplist(gv_id, [C,D], [FromId,ToId]),
  maplist(shacl_node_label, Ps, PLabels),
  atomics_to_string(PLabels, "/", PsLabel),
  gv_edge(Out, FromId, ToId, [label(PsLabel)]).





% HELPERS %

%! property_class(+Property, -C, +G:rdf_graph) is det.

property_class(Property, C, G) :-
  rdf_triple(Property, sh:class, C, G).
property_class(Property, C, G) :-
  rdf_triple_list_member(Property, sh:or, S, G),
  rdf_triple(S, sh:class, C, G).



%! property_path(+Property, -Ps:list, +G:rdf_graph) is det.

property_path(Property, Ps, G) :-
  rdf_triple(Property, sh:path, Path, G),
  (   rdf_triple(Path, rdf:type, rdf:'List', G)
  ->  aggregate_all(set(P), rdf_list_member(P, Path, G), Ps),
      assertion(Ps = [_,_|_])
  ;   Ps = [Path]
  ).



%! shacl_class(-C:iri, +G:rdf_graph) is nondet.

shacl_class(C, G) :-
  rdf_triple(C, owl:oneOf, _, G).
%shacl_class(C, G) :-
%  rdf_triple(_, rdf:type, C, G).
shacl_class(C, G) :-
  rdf_triple(C, rdfs:subClassOf, _, G).
shacl_class(C, G) :-
  rdf_triple(_, rdfs:subClassOf, C, G).
shacl_class(C, G) :-
  rdf_triple(Shape, rdf:type, sh:'NodeShape', G),
  rdf_triple(Shape, sh:targetClass, C, G).



%! shacl_edge(-Edge:edge, +G:rdf_graph) is nondet.

shacl_edge(edge(C,[P],D), G) :-
  rdf_equal(P, rdfs:subClassOf),
  rdf_triple(C, P, D, G).
shacl_edge(edge(C,Ps,D), G) :-
  shacl_property_path_class(C, Ps-D, G).



%! shacl_node_label(+Term:rdf_term, -Label:string) is det.

shacl_node_label(Term, Label) :-
  string_phrase(rdf_dcg_term(Term), Label0),
  gv_html_replace(Label0, Label).



%! shacl_property_path_class(+C:iri, -Pair:pair(list(iri),iri),
%!                           +G:rdf_graph) is nondet.

shacl_property_path_class(C, Ps-D, G) :-
  rdf_triple(Shape, sh:targetClass, C, G),
  rdf_triple(Shape, sh:property, Property, G),
  property_class(Property, D, G),
  property_path(Property, Ps, G).



%! shacl_property_path_datatype(+C:iri, -Pair:pair(list(iri),iri),
%!                              +G:rdf_graph) is nondet.

shacl_property_path_datatype(C, Ps-D, G) :-
  rdf_triple(Shape, sh:targetClass, C, G),
  rdf_triple(Shape, sh:property, Property, G),
  property_path(Property, Ps, G),
  rdf_triple(Property, sh:datatype, D, G).
