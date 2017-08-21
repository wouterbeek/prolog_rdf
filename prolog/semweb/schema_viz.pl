:- module(
  schema_viz,
  [
    schema_viz/1 % +File
  ]
).

/** <module> Schema visualization

@author Wouter Beek
@version 2017/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(file_ext)).
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_ext)).

:- rdf_meta
   schema_viz(r, +).





%! schema_viz(+File:atom) is det.

schema_viz(File) :-
  rdf_snap(schema_viz_(File, svg)).

schema_viz_(File1, Ext) :-
  file_name_extension(Base, _, File1),
  maplist(file_name_extension(Base), [dot,Ext], [File2,File3]),
  rdf_retractall,
  rdf_load2(File1),
  once(rdf_graph(G)),
  call_to_file(File2, schema_viz(G), [compression(none)]),
  run_process(dot, ['-T',Ext,file(File2),'-o',file(File3)]),
  delete_file(File2),
  rdf_retractall.

% Nodes can be described in multiple vocabularies: OWL, SHACL, RDF(S).
% We therefore first group all nodes with some description, and then
% generate nodes for each one in sequence.
schema_viz(G, Out, Meta, Meta) :-
  viz_top(Out),
  aggregate_all(set(Class), class(Class, G), Classes),
  maplist(viz_class(Out, G), Classes),
  aggregate_all(set(Edge), edge(Edge, G), Edges),
  maplist(viz_edge(Out), Edges),
  viz_bottom(Out).

viz_top(Out) :-
  format(Out, 'digraph G {\n', []).

viz_class(Out, G, Class1) :-
  rdf(trp, Class1, owl:oneOf, List, G), !,
  rdf_list(List, Values),
  graphviz_hash(Class1, NodeId),
  format(Out, '  ~a [label=<<TABLE>\n', [NodeId]),
  graphviz_iri(Class1, Class2),
  format(Out, '    <TR><TD><B>~a</B></TD></TR>\n', [Class2]),
  forall(
    member(Value, Values),
    format(Out, '    <TR><TD>~a</TD></TR>\n', [Value])
  ),
  format(Out, '  </TABLE>>,shape="node"];\n', []).
viz_class(Out, G, Class) :-
  viz_class_top(Out, Class),
  aggregate_all(set(Pair), pp(Class, Pair, G), Pairs),
  maplist(viz_class_pp(Out), Pairs),
  viz_class_bottom(Out).

viz_class_top(Out, Class1) :-
  graphviz_hash(Class1, ClassId),
  format(Out, '  ~a [label=<<TABLE>\n', [ClassId]),
  graphviz_iri(Class1, Class2),
  format(Out, '    <TR><TD COLSPAN="2"><B>~a</B></TD></TR>\n', [Class2]).

viz_class_pp(Out, Segments-Target1) :-
  segments_sequence(Segments, Sequence),
  graphviz_iri(Target1, Target2),
  format(Out, '    <TR><TD>~a</TD><TD>~a</TD></TR>\n', [Sequence,Target2]).

viz_class_bottom(Out) :-
  format(Out, '  </TABLE>>,shape="node"];\n', []).

viz_edge(Out, edge(Class1,⊆,Class2)) :- !,
  maplist(graphviz_hash, [Class1,Class2], [NodeId1,NodeId2]),
  format(Out, '  ~a -> ~a [arrowHead="empty"];\n', [NodeId1,NodeId2]).
viz_edge(Out, edge(Class1,Segments,Class2)) :-
  segments_sequence(Segments, Sequence),
  maplist(graphviz_hash, [Class1,Class2], [NodeId1,NodeId2]),
  format(Out, '  ~a -> ~a [label=<~a>];\n', [NodeId1,NodeId2,Sequence]).

viz_bottom(Out) :-
  format(Out, "}\n", []).



class(Class, G) :-
  rdf(trp, Class, owl:oneOf, _, G).
class(Class, G) :-
  rdf(trp, Class, rdfs:subClassOf, _, G).
class(Class, G) :-
  rdf(trp, _, rdfs:subClassOf, Class, G).
class(Class, G) :-
  rdf(trp, Shape, rdf:type, sh:'NodeShape', G),
  rdf(trp, Shape, sh:targetClass, Class, G).

pp(Class, Segments-Target, G) :-
  rdf(trp, Shape, sh:targetClass, Class, G),
  rdf(trp, Shape, sh:property, Property, G),
  rdf(trp, Property, sh:path, Path, G),
  path_segments(Path, Segments),
  target(Property, Target, G).

target(Property, Datatype, G) :-
  rdf(trp, Property, sh:datatype, Datatype, G).
target(Property, Class, G) :-
  rdf(trp, Property, sh:class, Class, G).

edge(edge(Class1,⊆,Class2), G) :-
  rdf(trp, Class1, rdfs:subClassOf, Class2, G).
edge(edge(Class1,Segments,Class2), G) :-
  rdf(trp, Shape, sh:targetClass, Class1, G),
  rdf(trp, Shape, sh:property, Property, G),
  rdf(trp, Property, sh:path, Path, G),
  path_segments(Path, Segments),
  rdf(trp, Property, sh:class, Class2, G).



% HELPERS %

graphviz_iri(Iri, Label) :-
  prefix_local_iri(Prefix, Local, Iri), !,
  atomic_list_concat([Prefix,Local], :, Label).
graphviz_iri(Iri, Iri).

path_segments(Path, Segments) :-
   rdf_list(Path, Segments), !.
path_segments(Segment, [Segment]).

segments_sequence(Segments1, Sequence) :-
  maplist(graphviz_iri, Segments1, Segments2),
  atomic_list_concat(Segments2, /, Sequence).
