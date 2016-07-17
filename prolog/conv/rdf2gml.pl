:- module(
  rdf2gml,
  [
    rdf2gml/1,       % +Source
    rdf2gml/2,       % +Source, +Opts
    rdf2gml_end/3,   % +NFile, +EFile, +GFile
    rdf2gml_end/4,   % +NFile, +EFile, +GFile, +Opts
    rdf2gml_start/4, % -NFile, -EFile, -GFile, -ExportOpts
    rdf2gml_start/5, % +Opts, -NFile, -EFile, -GFile, -ExportOpts
    rdf2gml_triple/6 % +EOut, +NOut, +S, +P, +O, +Opts
  ]
).

/** <module> RDF-2-GML

Grammar
=======

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

Keys
====

  - `comment` `string` Defines a comment embedded in a GML
  file. Comments are ignored by the application.

  - `id` `int` Defines an identification number for an object. This is
  usually used to represent pointers.
  
  - `label` `string` Defines a label attached to an object.

@author Wouter Beek
@version 2016/02-2016/04, 2016/06-2016/07
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(option)).
:- use_module(library(os/compress_ext)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/io)).
:- use_module(library(os/process_ext)).
:- use_module(library(os/thread_counter)).
:- use_module(library(pairs)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(thread)).
:- use_module(library(uuid)).
:- use_module(library(yall)).

:- meta_predicate
    gml_label(4, +, -, +).

:- thread_local
   node_id0/3.





%! rdf2gml(+Source) is det.
%! rdf2gml(+Source, +Opts) is det.
%
% Source can be any source, but Sink must be a file.  The reason for
% this is that GML cannot be written in-stream because vertices and
% edges are stored separately.

rdf2gml(Source) :-
  rdf2gml(Source, []).


rdf2gml(Source, Opts) :-
  rdf2gml_start(Opts, NFile, EFile, GFile, ExportOpts),
  call_onto_streams(
    Source, NFile, EFile,
    rdf2gml_stream0(Opts, ExportOpts),
    Opts, [compression(false)]
  ),
  rdf2gml_end(NFile, EFile, GFile, Opts).


rdf2gml_stream0(Opts, ExportOpts, In, Meta, Meta, NOut, EOut) :-
  rdf_call_on_tuples_stream(In, rdf2gml_triple0(NOut, EOut, ExportOpts), Meta, Opts).


rdf2gml_triple0(NOut, EOut, ExportOpts, _, S, P, O, _) :-
  rdf2gml_triple(NOut, EOut, S, P, O, ExportOpts).



%! rdf2gml_end(+NFile, +EFile, +GFile) is det.
%! rdf2gml_end(+NFile, +EFile, +GFile, +Opts) is det.
%
% The following options are supported:
%
%   * compression(+boolean) Whether or not the GML file is compressed.
%   Default is `true`.

rdf2gml_end(NFile, EFile, GFile) :-
  rdf2gml_end(NFile, EFile, GFile, []).


rdf2gml_end(NFile, EFile, GFile, Opts) :-
  SinkOpts = [compression(false)],
  % Sort the nodes and edges to ensure there are no duplicates.
  concurrent_maplist(sort_file, [NFile,EFile]),
  % Concatenate the nodes and edges into one file.
  call_to_stream(GFile, [Out]>>format(Out, "graph [~n  directed 1~n", []), SinkOpts),
  format(atom(CatCmd), "cat ~a ~a >> ~a", [NFile,EFile,GFile]),
  run_process(sh, ['-c',CatCmd]),
  call_to_stream(GFile, [Out]>>format(Out, "]~n", []), SinkOpts),
  (option(compression(true), Opts, true) -> compress_file(GFile) ; true),
  concurrent_maplist(delete_file, [NFile,EFile]),
  delete_thread_counter(node_id),
  retractall(node_id0(_,_,_)).



%! rdf2gml_start(-NFile, -EFile, -GFile, -ExportOpts) is det.
%! rdf2gml_start(+Opts, -NFile, -EFile, -GFile, -ExportOpts) is det.
%
% The following options are supported:
%
%   * base_name(+atom) The base name of the written files.  The
%   default is a UUID.
%
%   * compression(+boolean) Whether or not the GML file is compressed.
%   Default is `true`.
%
%   * export_options(+dict) Options dictionary passed to GML writers.
%   This includes the following options:
%
%     * edge_label_printer(+callable) The DCG writer for GML edges.
%     The default is dcg_print_predicate//2.
%
%     * node_label_printer(+callable) The DCG writer for GML nodes.
%     The default is dcg_print_node//2.

rdf2gml_start(NFile, EFile, GFile, ExportOpts) :-
  rdf2gml_start([], NFile, EFile, GFile, ExportOpts).


rdf2gml_start(Opts, NFile, EFile, GFile, ExportOpts2) :-
  option(export_options(ExportOpts1), Opts, _{}),
  q_print:dcg_print_default_options(DefExportOpts),
  merge_dicts(DefExportOpts, ExportOpts1, ExportOpts2),
  (option(base_name(Base), Opts) -> true ; uuid(Base)),
  atomic_list_concat([Base,nodes,tmp], ., NFile),
  atomic_list_concat([Base,edges,tmp], ., EFile),
  (option(compression(true), Opts, true) -> T = [gz] ; T = []),
  atomic_list_concat([Base,graph,gml|T], ., GFile),
  create_thread_counter(node_id).



%! rdf2gml_triple(+EOut, +NOut, +S, +P, +O, +ExportOpts) is det.

rdf2gml_triple(NOut, EOut, S, P, O, ExportOpts) :-
  maplist(gml_node(NOut, ExportOpts), [S,O], [NId1,NId2]),
  gml_edge(EOut, NId1, P, NId2, ExportOpts).



%! gml_edge(+EOut, +NId1, +P, +NId2, +ExportOpts) is det

gml_edge(EOut, NId1, P, NId2, ExportOpts) :-
  get_dict(edge_label_printer, ExportOpts, Dcg_4, dcg_print_predicate),
  gml_label(Dcg_4, P, EL, ExportOpts),
  format(EOut, "  edge [ label \"~a\" source ~d target ~d ]~n", [EL,NId1,NId2]).



%! gml_encode_label// is det.

% ASCII
gml_encode_label, [C] -->
  [C],
  {gml_unencoded(C)}, !,
  gml_encode_label.
% &-encoding
gml_encode_label, Escape -->
  [C], !,
  {format(codes(Escape), "&#~d", [C])},
  gml_encode_label.
gml_encode_label --> [].



%! gml_label(:Dcg_4, +T, -Label, +ExportOpts) is det.

gml_label(Dcg_4, T, Lbl, ExportOpts) :-
  phrase(dcg_call(Dcg_4, T, ExportOpts), Cs1),
  phrase(gml_encode_label, Cs1, Cs2),
  atom_codes(Lbl, Cs2).



%! gml_node(+Out, +ExportOpts, +N, -Id) is det.

gml_node(Out, Opts, N, Id) :-
  gml_node0(N, Id, Lbl, Opts),
  format(Out, "  node [ id ~d label \"~a\" ]~n", [Id,Lbl]).

gml_node0(N, Id, Lbl, _) :-
  node_id0(N, Id, Lbl), !.
gml_node0(N, Id, Lbl, Opts) :-
  get_dict(node_label_printer, Opts, Dcg_4, dcg_print_node),
  inc_thread_counter(node_id, Id),
  gml_label(Dcg_4, N, Lbl, Opts),
  assert(node_id0(N,Id,Lbl)),
  debug(conv(rdf2gml), "Added node ID: ~D (~w)", [Id,N]).



%! gml_unencoded(+Code) is semidet.
%
% ASCII characters, excluding double quote (34) and ampersand (38).

gml_unencoded(C) :-
  between(0,  33,  C), !.
gml_unencoded(C) :-
  between(35, 37,  C), !.
gml_unencoded(C) :-
  between(39, 127, C).
