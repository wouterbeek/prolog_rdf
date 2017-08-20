:- module(
  shacl_viz,
  [
    shacl_viz/2 % +G, +Base
  ]
).

/** <module> SHACLE visualization

@author Wouter Beek
@version 2017/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(file_ext)).
:- use_module(library(lists)).
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(semweb/rdf_print)).

:- rdf_meta
   shacl_viz(r, +).





%! shacl_viz(+G:atom, +Base:atom) is det.

shacl_viz(G, Base) :-
  Ext = svg,
  maplist(file_name_extension(Base), [dot,Ext], [File1,File2]),
  call_to_file(File1, shacl_viz_(G), [compression(none)]),
  run_process(dot, ['-T',Ext,file(File1),'-o',file(File2)]),
  %delete_file(File1),
  true.

shacl_viz_(G, Out, Meta, Meta) :-
  format(Out, 'digraph G {\n', []),
  forall(
    rdf(trp, Shape, rdf:type, sh:'NodeShape', G),
    shape_viz_(Out, Shape, G)
  ),
  forall(
    rdf(trp, Class, owl:oneOf, List, G),
    owl_viz_(Class, List)
  ),
  forall(
    rdf(trp, C1, rdfs:subClassOf, C2, G),
    (
      maplist(graphviz_hash, [C1,C2], [NodeId1,NodeId2]),
      format(Out, '  ~a -> ~a [arrowHead="empty"];\n', [NodeId1,NodeId2])
    )
  ),
  format(Out, "}\n", []).

shape_viz_(Out, Shape, G) :-
  rdf(trp, Shape, sh:targetClass, C1, G),
  graphviz_hash(C1, NodeId1),
  format(Out, '  ~a [label=<<TABLE>\n', [NodeId1]),
  graphviz_iri(C1, C2),
  format(Out, '    <TR><TD COLSPAN="2"><B>~a</B></TD></TR>\n', [C2]),
  aggregate_all(
    set(Sequence-D2),
    (
      rdf(trp, Shape, sh:property, Property, G),
      rdf(trp, Property, sh:datatype, D1, G),
      graphviz_iri(D1, D2),
      rdf(trp, Property, sh:path, Path, G),
      path_label(Path, Sequence)
    ),
    Pairs
  ),
  forall(
    member(Sequence-D, Pairs),
    format(Out, '    <TR><TD>~a</TD><TD>~a</TD></TR>\n', [Sequence,D])
  ),
  format(Out, '  </TABLE>>,shape="node"];\n', []),
  forall(
    (
      rdf(trp, Shape, sh:property, Property, G),
      rdf(trp, Property, sh:class, C3, G)
    ),
    (
      graphviz_hash(C3, NodeId2),
      rdf(trp, Property, sh:path, Path, G),
      path_label(Path, Sequence),
      format(Out, '  ~a -> ~a [label=<~a>];\n', [NodeId1,NodeId2,Sequence])
    )
  ).

owl_viz_(Class, List) :-
  rdf_list(List, Values),
  graphviz_iri(Class, NodeId),
  format(Out, '  ~a [label=<<UL>\n', [NodeId]),
  forall(
    member(Value, Values),
    format(Out, '    <LI>~a</LI>\n', [Value])
  ),
  format(Out, '  </UL>>];\n', []).

graphviz_iri(Iri, Label) :-
  prefix_local_iri(Prefix, Local, Iri), !,
  atomic_list_concat([Prefix,Local], :, Label).
graphviz_iri(Iri, Iri).

path_label(Path, Sequence) :-
  (rdf_list(Path, Segments1) -> true ; Segments1 = [Path]),
  maplist(graphviz_iri, Segments1, Segments2),
  atomic_list_concat(Segments2, /, Sequence).
