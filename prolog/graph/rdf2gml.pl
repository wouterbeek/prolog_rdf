:- module(
  rdf2gml,
  [
    rdf2gml/2, % +UriSpec, +FileSpec
    rdf2gml/3  % +UriSpec, +FileSpec, +Options
  ]
).

/** <module> RDF-2-GML

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
@version 2016/02, 2017/01, 2017/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(option)).
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(thread)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(uuid)).

:- meta_predicate
    gml_label(4, +, -),
    gml_node(+, 4, +, -),
    gml_triple(+, 4, +, 4, +),
    gml_triples(+, 4, +, 4, +, +).





%! rdf2gml(+UriSpec:compound, +FileSpec:compound) is det.
%! rdf2gml(+UriSpec:compound, +FileSpec:compound,
%!         +Options:list(compound)) is det.
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

rdf2gml(UriSpec, FileSpec) :-
  rdf2gml(UriSpec, FileSpec, []).


rdf2gml(UriSpec, FileSpec, Options) :-
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
    forall(
      call_on_rdf(UriSpec, gml_triples(NOut, NPrinter_2, EOut, EPrinter_2)),
      true
    ),
    (
      close(EOut),
      close(NOut)
    )
  ),
  % LC_ALL=C sort -o identity-explicit-sorted.tsv identity-explicit.tsv
  concurrent_maplist(sort_file, [NFile,EFile]),
  call_to_file(FileSpec, write_(NFile,EFile)).

write_(NFile, EFile, Out, Meta, Meta) :-
  format(Out, "graph [\n  directed 1\n", []),
  call_on_uri(NFile, {Out}/[In,Meta,Meta]>>copy_stream_data(In, Out)),
  call_on_uri(EFile, {Out}/[In,Meta,Meta]>>copy_stream_data(In, Out)),
  format(Out, "]\n", []),
  concurrent_maplist(delete_file, [NFile,EFile]).

sort_file(File) :-
  run_process(sort, ['-u','-o',file(File),file(File)], [env(['LC_ALL'='C'])]).

gml_triples(NOut, NPrinter_2, EOut, EPrinter_2, Tuples, _) :-
  maplist(gml_triple(NOut, NPrinter_2, EOut, EPrinter_2), Tuples).

gml_triple(NOut, NPrinter_2, EOut, EPrinter_2, Tuple1) :-
  rdf_clean_tuple(Tuple1, Tuple2),
  rdf_tuple_triple(Tuple2, rdf(S,P,O)),
  maplist(gml_node(NOut, NPrinter_2), [S,O], [NId1,NId2]),
  gml_label(EPrinter_2, P, ELabel),
  format(EOut, "  edge [ label \"~s\" source ~a target ~a ]\n",
         [ELabel,NId1,NId2]).



%! gml_node(+NOut:stream, :NPrinter_2, +N:term, -NId:atom) is det.

gml_node(NOut, NPrinter_2, N, NId) :-
  md5(N, NId),
  gml_label(NPrinter_2, N, NLabel),
  format(NOut, "  node [ id ~a label \"~s\" ]\n", [NId,NLabel]).



%! gml_label(:Printer_2, +Term:term, -Label:string) is det.

gml_label(Printer_2, Term, Label) :-
  phrase(dcg_call(Printer_2, Term), Codes1),
  phrase(gml_encode_label, Codes1, Codes2),
  string_codes(Label, Codes2).

% ASCII
gml_encode_label, [Code] -->
  [Code],
  {gml_unencoded(Code)}, !,
  gml_encode_label.
% &-encoding
gml_encode_label, Escape -->
  [Code], !,
  {format(codes(Escape), "&#~d", [Code])},
  gml_encode_label.
gml_encode_label --> [].

% ASCII characters, excluding double quote (34) and ampersand (38).
gml_unencoded(Code) :-
  between(0,  33,  Code), !.
gml_unencoded(Code) :-
  between(35, 37,  Code), !.
gml_unencoded(Code) :-
  between(39, 127, Code).
