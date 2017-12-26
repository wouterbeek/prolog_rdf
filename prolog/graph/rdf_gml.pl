:- module(
  rdf_gml,
  [
    rdf2gml/2,       % +File1, +File2
    rdf2gml/3,       % +File1, +File2, +Options
    rdf_export_gml/1 % +Triples
  ]
).

/** <module> RDF GML

Automatic conversion from RDF to the Graph Markup Language (GML).

# Grammar

```bnf
GML        ::= List
List       ::= (whitespace* Key whitespace+ Value)*
Value      ::= Integer | Real | String | [ List ]
Key        ::= [ a-z A-Z ] [ a-z A-Z 0-9 ]*
Integer    ::= sign digit+
Real       ::= sign digit* . digit* mantissa
String     ::= " instring "
sign       ::= empty | + | -
digit      ::= [0-9]
Mantissa   ::= empty | E sign digit
instring   ::= ASCII - {&,"} | & character+ ;
whitespace ::= space | tabulator | newline
```

@author Wouter Beek
@version 2016/02, 2017/01-2017/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(file_ext)).
:- use_module(library(graph/gml)).
:- use_module(library(hash_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(thread)).
:- use_module(library(uuid)).
:- use_module(library(zlib)).

:- meta_predicate
    gml_label(4, +, -),
    gml_node(+, 4, +, -),
    gml_triple(+, 4, +, 4, +),
    gml_triples(+, 4, +, 4, +, +).





%! rdf2gml(+File1:atom, +File2:atom) is det.
%! rdf2gml(+File1:atom, +File2:atom, +Options:list(compound)) is det.
%
% Source can be any source, but Sink must be a file.  The reason for
% this is that GML cannot be written in-stream because vertices and
% edges are stored separately.
%
% The following options are supported:
%
%   * edge_label_printer(+callable)
%
%     The DCG writer for GML edges.  Default is rdf_dcg_predicate//1.
%
%   * node_label_printer(+callable)
%
%     The DCG writer for GML nodes.  Default is rdf_dcg_node//1.

rdf2gml(File1, File2) :-
  rdf2gml(File1, File2, []).


rdf2gml(File1, File2, Options) :-
  uuid(Base),
  atomic_list_concat([Base,nodes,tmp], ., NFile),
  atomic_list_concat([Base,edges,tmp], ., EFile),
  option(node_label_printer(NPrinter_2), Options, rdf_dcg_node),
  option(edge_label_printer(EPrinter_2), Options, rdf_dcg_predicate),
  setup_call_cleanup(
    (
      open(EFile, write, EOut),
      open(NFile, write, NOut)
    ),
    rdf_deref(File1, gml_triples(NOut, NPrinter_2, EOut, EPrinter_2)),
    (
      close(EOut),
      close(NOut)
    )
  ),
  % LC_ALL=C sort -o identity-explicit-sorted.tsv identity-explicit.tsv
  concurrent_maplist(sort_file, [NFile,EFile]),
  setup_call_cleanup(
    open(File2, write, Out),
    (
      debug_format(gml, Out, "graph ["),
      debug_format(gml, Out, "  directed 1"),
      cat(Out, [NFile,EFile]),
      debug_format(gml, Out, "]"),
      concurrent_maplist(delete_file, [NFile,EFile])
    ),
    close(Out)
  ).

gml_triples(NOut, NPrinter_2, EOut, EPrinter_2, Tuples, _) :-
  maplist(gml_triple(NOut, NPrinter_2, EOut, EPrinter_2), Tuples).

gml_triple(NOut, NPrinter_2, EOut, EPrinter_2, Tuple1) :-
  rdf_clean_tuple(Tuple1, Tuple2),
  rdf_tuple_triple(Tuple2, rdf(S,P,O)),
  maplist(gml_node(NOut, NPrinter_2), [S,O], [NId1,NId2]),
  gml_label(EPrinter_2, P, ELabel),
  debug_format(
    gml,
    EOut,
    '  edge [ label "~s" source ~a target ~a ]',
    [ELabel,NId1,NId2]
  ).



%! gml_node(+NOut:stream, :NPrinter_2, +N:term, -NId:atom) is det.

gml_node(NOut, NPrinter_2, N, NId) :-
  md5(N, NId),
  gml_label(NPrinter_2, N, NLabel),
  debug_format(gml, NOut, '  node [ id ~a label "~s" ]', [NId,NLabel]).



%! gml_label(:Printer_2, +Term:term, -Label:string) is det.

gml_label(Printer_2, Term, Label) :-
  phrase(dcg_call(Printer_2, Term), Codes1),
  phrase(gml_encode_label, Codes1, Codes2),
  string_codes(Label, Codes2).

% ASCII
% ASCII characters, excluding double quote (34) and ampersand (38).
gml_encode_label, [Code] -->
  dcg_between(0,  33,  Code),
  gml_encode_label.
gml_encode_label, [Code] -->
  dcg_between(35, 37,  Code),
  gml_encode_label.
gml_encode_label, [Code] -->
  dcg_between(39, 127, Code),
  gml_encode_label.
% &-encoding
gml_encode_label, Escape -->
  [Code], !,
  {format(codes(Escape), "&#~d", [Code])},
  gml_encode_label.
gml_encode_label --> [].



%! rdf_export_gml(+Triples:list(rdf_triple)) is det.

rdf_export_gml(Triples) :-
  aggregate_all(
    set(Term),
    (
      member(Triple, Triples),
      rdf_triple_term(Triple, Term)
    ),
    Terms
  ),
  maplist(rdf_export_gml_node_, Terms),
  maplist(rdf_export_gml_edge_, Triples).

rdf_export_gml_node_(Term) :-
  gml_id(Term, Id),
  string_phrase(rdf_dcg_term(Term), Label),
  gml_node(current_output, Id, [label(Label)]).

rdf_export_gml_edge_(rdf(S,P,O)) :-
  maplist(gml_id, [S,O], [SId,OId]),
  string_phrase(rdf_dcg_iri(P), PLabel),
  gml_edge(current_output, SId, OId, [label(PLabel)]).
