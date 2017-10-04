:- module(
  schema_viz,
  [
    schema_viz/2,     % +Format, -Out
    schema_viz_file/1 % +File
  ]
).

/** <module> Schema visualization

@author Wouter Beek
@version 2017/08-2017/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(graph/dot)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(zlib)).

:- rdf_meta
   schema_viz(r, +).





%! schema_viz(+Format, -Out:stream) is det.

schema_viz(Format, ProcOut) :-
  setup_call_cleanup(
    graphviz(dot, ProcIn, Format, ProcOut),
    schema_viz_stream(ProcIn),
    close(ProcIn)
  ).

% Nodes can be described in multiple vocabularies: OWL, SHACL, RDF(S).
% We therefore first group all nodes with some description, and then
% generate nodes for each one in sequence.
schema_viz_stream(Out) :-
  viz_top(Out),
  aggregate_all(set(Class), class(Class), Classes),
  maplist(viz_class(Out), Classes),
  aggregate_all(set(Edge), edge(Edge), Edges),
  maplist(viz_edge(Out), Edges),
  viz_bottom(Out).

viz_top(Out) :-
  format_debug(dot, Out, "digraph G {").

viz_class(Out, Class1) :-
  aggregate_all(
    set(Value),
    rdf_list_member(Class1, owl:oneOf, Value),
    Values
  ),
  Values \== [], !,
  dot_id(Class1, Id),
  format_debug(dot, Out, "  ~a [label=<<TABLE>", [Id]),
  dot_iri(Class1, Class2),
  format_debug(dot, Out, "    <TR><TD><B>~a</B></TD></TR>", [Class2]),
  forall(
    member(Value, Values),
    format_debug(dot, Out, "    <TR><TD>~a</TD></TR>", [Value])
  ),
  format_debug(dot, Out, "  </TABLE>>,shape=\"node\"];").
viz_class(Out, Class) :-
  viz_class_top(Out, Class),
  aggregate_all(set(Pair), pp(Class, Pair), Pairs),
  maplist(viz_class_pp(Out), Pairs),
  viz_class_bottom(Out).

viz_class_top(Out, Class1) :-
  dot_id(Class1, ClassId),
  format_debug(dot, Out, "  ~a [label=<<TABLE>", [ClassId]),
  dot_iri(Class1, Class2),
  format_debug(
    dot,
    Out,
    "    <TR><TD COLSPAN=\"2\"><B>~a</B></TD></TR>",
    [Class2]
  ).

viz_class_pp(Out, Segments-Target1) :-
  segments_sequence(Segments, Sequence),
  dot_iri(Target1, Target2),
  format_debug(
    dot,
    Out,
    "    <TR><TD>~a</TD><TD>~a</TD></TR>",
    [Sequence,Target2]
  ).

viz_class_bottom(Out) :-
  format_debug(dot, Out, "  </TABLE>>,shape=\"node\"];").



%! viz_edge(+Out:stream, +Edge:compound) is det.

viz_edge(Out, edge(Class1,⊆,Class2)) :- !,
  maplist(dot_id, [Class1,Class2], [Id1,Id2]),
  format_debug(dot, Out, "  ~a -> ~a [arrowhead=\"onormal\"];", [Id1,Id2]).
viz_edge(Out, edge(Class1,Segments,Class2)) :-
  segments_sequence(Segments, Sequence),
  maplist(dot_id, [Class1,Class2], [Id1,Id2]),
  format_debug(dot, Out, "  ~a -> ~a [label=<~a>];", [Id1,Id2,Sequence]).

viz_bottom(Out) :-
  format_debug(dot, Out, "}").

class(Class) :-
  rdf(Class, owl:oneOf, _).
class(Class) :-
  rdf(Class, rdfs:subClassOf, _).
class(Class) :-
  rdf(_, rdfs:subClassOf, Class).
class(Class) :-
  rdf(Shape, rdf:type, sh:'NodeShape'),
  rdf(Shape, sh:targetClass, Class).


%! pp(+Class:atom, -Pair:pair(list(atom),atom)) is nondet.
%
% Enumerates property path-target pairs for the given Class.

pp(Class, Segments-Target) :-
  rdf(Shape, sh:targetClass, Class),
  rdf(Shape, sh:property, Property),
  rdf(Property, sh:path, Path),
  path_segments(Path, Segments),
  target(Property, Target).

target(Property, Datatype) :-
  rdf(Property, sh:datatype, Datatype).
target(Property, Class) :-
  rdf(Property, sh:class, Class).

edge(edge(Class1,⊆,Class2)) :-
  rdf(Class1, rdfs:subClassOf, Class2).
edge(edge(Class1,Segments,Class2)) :-
  rdf(Shape, sh:targetClass, Class1),
  rdf(Shape, sh:property, Property),
  rdf(Property, sh:path, Path),
  path_segments(Path, Segments),
  rdf(Property, sh:class, Class2).

dot_iri(Iri, Label) :-
  prefix_local_iri(Prefix, Local, Iri), !,
  atomic_list_concat([Prefix,Local], :, Label).
dot_iri(Iri, Iri).

% A property path is either composed on one segment or of a sequence
% (`rdf:List') of segments.
path_segments(Path, Segments) :-
  aggregate_all(
    set(Segment),
    rdf_list_member(Path, Segment),
    Segments
  ),
  Segments \== [], !.
path_segments(Segment, [Segment]).

segments_sequence(Segments1, Sequence) :-
  maplist(dot_iri, Segments1, Segments2),
  atomic_list_concat(Segments2, /, Sequence).



%! schema_viz_file(+File:atom) is det.

schema_viz_file(File1) :-
  file_name_extension(Base, _, File1),
  file_name_extension(Base, svgz, File2),
  setup_call_cleanup(
    gzopen(File2, write, Out),
    rdf_transaction((
        rdf_load2(File1),
        schema_viz(svg, ProcOut),
        copy_stream_data(ProcOut, Out)
      ), _, [snapshot(true)]
    ),
    close(Out)
  ).
