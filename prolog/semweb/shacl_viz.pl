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
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(semweb/rdf_print)).





%! shacl_viz(+G:atom, +Base:atom) is det.

shacl_viz(G, Base) :-
  Ext = svg,
  maplist(file_name_extension(Base), [dot,Ext], [File1,File2]),
  call_to_file(File1, shacl_viz_(G), [compression(none)]),
  run_process(dot, ['-T',Ext,file(File1),'-o',file(File2)]),
  delete_file(File1).

shacl_viz_(G, Out, Meta, Meta) :-
  format(Out, "digraph G {\n", []),
  forall(
    rdf(trp, Shape, rdf:type, sh:'NodeShape', G),
    shape_viz_(Out, Shape, G)
  ),
  format(Out, "}\n", []).

shape_viz_(Out, Shape, G) :-
  rdf(trp, Shape, sh:targetClass, C1, G),
  graphviz_hash(C1, NodeId1),
  format(Out, '  ~a [label=<<TABLE>\n', [NodeId1]),
  
  graphviz_iri(C1, C2),
  format(Out, '    <TR><TD><B>~a</B></TD></TR>\n', [C2]),
  
  aggregate_all(
    set(P2-D2),
    (
      rdf(trp, Shape, sh:property, Property, G),
      rdf(trp, Property, sh:datatype, D1, G),
      rdf(trp, Property, sh:path, P1, G),
      maplist(graphviz_iri, [P1,D1], [P2,D2])
    ),
    Pairs
  ),
  forall(
    member(P-D, Pairs),
    format(Out, '    <TR><TD>~a ~a</TD></TR>\n', [P,D])
  ),
  
  format(Out, '  </TABLE>>,shape="node"];\n', []),
  
  forall(
    (
      rdf(trp, Shape, sh:property, Property, G),
      rdf(trp, Shape, sh:class, C3, G)
    ),
    (
      graphviz_hash(C3, NodeId2),
      rdf(trp, Shape, sh:path, P3, G),
      graphviz_iri(P3, P4),
      format(Out, '  ~a -> ~a [label=<~a>];\n', [NodeId1,NodeId2,P4])
    )
  ).

graphviz_iri(Iri, Label) :-
  prefix_local_iri(Prefix, Local, Iri), !,
  atomic_list_concat([Prefix,Local], :, Label).
graphviz_iri(Iri, Iri).
