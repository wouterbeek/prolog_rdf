:- module(
  shacl,
  [
    shacl_assert_class/1, % +Entry
    shacl_assert_class/2, % +Entry, +G
    shacl_export/2        % +Out, +G
  ]
).

/** <module> SHACL

@author Wouter Beek
@version 2017/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(graph/gv)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/schema_viz)).
:- use_module(library(yall)).

:- rdf_meta
   shacl_assert_class(+, r),
   shacle_export(+, r).





%! shacl_assert_class(+Entry:compound) is det.
%! shacl_assert_class(+Entry:compound, +G:iri) is det.

shacl_assert_class(Entry) :-
  rdf_default_graph(G),
  shacl_assert_class(Entry, G).


shacl_assert_class(C-Groups, G) :-
  atom_concat(C, 'Shape', Shape),
  rdf_assert(Shape, rdf:type, sh:'NodeShape', G),
  rdf_assert(Shape, sh:targetClass, C, G),
  rdf_assert_list(Shape, sh:ignoredProperties, [rdf:type], G),
  maplist(shacl_assert_property(C, G), Groups).

shacl_assert_property(C, G, P-Os) :-
  rdf_create_bnode(BNode),
  rdf_assert(C, sh:property, BNode, G),
  rdf_assert(BNode, sh:path, P, G),
  (   Os = [O]
  ->  rdf_assert(BNode, sh:class, O, G)
  ;   maplist(shacl_assert_object(G), Os, BNodes),
      rdf_assert_list(BNode, sh:or, BNodes, G)
  ).

shacl_assert_object(G, O, BNode) :-
  rdf_create_bnode(BNode),
  rdf_assert(BNode, sh:class, O, G).



%! shacl_export(+Out:stream, +G:iri) is det.

shacl_export(Out, G) :-
  % top of graph
  format_debug(dot, Out, "digraph shacl {"),
  % Nodes can be described in multiple vocabularies: OWL, SHACL,
  % RDF(S).  We therefore first group all nodes with some description,
  % and then generate nodes for each one in sequence.
  aggregate_all(set(C), shacl_class(C, G), Cs),
  maplist({Out,G}/[C]>>shacl_export_class(Out, C, G), Cs),
  aggregate_all(set(Edge), shacl_edge(Edge, G), Edges),
  maplist(export_class_edge(Out), Edges),
  % bottom of graph
  format_debug(dot, Out, "}").



%! shacl_export_class(+Out:stream, +C:atom, +G:atom) is det.

% The instances of class `C' form an OWL value list.
shacl_export_class(Out, C, G) :-
  aggregate_all(
    set(Value),
    rdf_triple_list_member(C, owl:oneOf, Value, G),
    Values
  ),
  Values \== [], !,
  dot_id(C, CId),
  % top of node
  format_debug(dot, Out, "  ~a [label=<<TABLE>", [CId]),
  iri_label(C, CLabel),
  format_debug(dot, Out, "    <TR><TD><B>~a</B></TD></TR>", [CLabel]),
  % middle of node
  forall(
    member(Value, Values),
    (
      iri_label(Value, ValueLabel),
      format_debug(dot, Out, "    <TR><TD>~a</TD></TR>", [ValueLabel])
    )
  ),
  % bottom of node
  format_debug(dot, Out, '  </TABLE>>,shape="none",URL="~a"];', [C]).
% The instances of class `C' are related to certain property/range
% pairs.
shacl_export_class(Out, C, G) :-
  dot_id(C, CId),
  % top of node
  format_debug(dot, Out, "  ~a [label=<<TABLE>", [CId]),
  iri_label(C, CLabel),
  format_debug(
    dot,
    Out,
    '    <TR><TD COLSPAN="2"><B>~a</B></TD></TR>',
    [CLabel]
  ),
  % middle of node
  aggregate_all(set(Pair), shacl_property_path_datatype(C, Pair, G), Pairs),
  forall(
    member(Ps-O, Pairs),
    (
      property_path_label(Ps, PsLabel),
      iri_label(O, OLabel),
      format_debug(
        dot,
        Out,
        "    <TR><TD>~a</TD><TD>~a</TD></TR>",
        [PsLabel,OLabel]
      )
    )
  ),
  % bottom of node
  format_debug(dot, Out, '  </TABLE>>,shape="none",URL="~a"];', [C]).
  




% HELPERS %

%! shacl_class(-C:iri, ?G:rdf_graph) is nondet.

shacl_class(C, G) :-
  rdf(C, owl:oneOf, _, G).
%shacl_class(C, G) :-
%  rdf(_, rdf:type, C, G).
shacl_class(C, G) :-
  rdf(C, rdfs:subClassOf, _, G).
shacl_class(C, G) :-
  rdf(_, rdfs:subClassOf, C, G).
shacl_class(C, G) :-
  rdf(Shape, rdf:type, sh:'NodeShape', G),
  rdf(Shape, sh:targetClass, C, G).



%! shacl_edge(-Edge:edge, ?G:rdf_graph) is nondet.

shacl_edge(edge(C,[rdfs:subClassOf],D), G) :-
  rdf(C, rdfs:subClassOf, D, G).
shacl_edge(edge(C,Ps,D), G) :-
  shacl_property_path_class(C, Ps-D, G).



%! shacl_property_path_class(+C:iri, -Pair:pair(list(iri),iri),
%!                           ?G:rdf_graph) is nondet.

shacl_property_path_class(C, Ps-D, G) :-
  rdf(Shape, sh:targetClass, C, G),
  rdf(Shape, sh:property, Property, G),
  rdf(Property, sh:path, Path, G),
  aggregate_all(set(P), rdf_list_member(P, Path, G), Ps),
  assertion(Ps \== []),
  rdf(Property, sh:class, D, G).



%! shacl_property_path_datatype(+C:iri, -Pair:pair(list(iri),iri),
%!                              ?G:rdf_graph) is nondet.

shacl_property_path_datatype(C, Ps-D, G) :-
  rdf(Shape, sh:targetClass, C, G),
  rdf(Shape, sh:property, Property, G),
  rdf(Property, sh:path, Path, G),
  aggregate_all(set(P), rdf_list_member(P, Path, G), Ps),
  assertion(Ps \== []),
  rdf(Property, sh:datatype, D, G).
