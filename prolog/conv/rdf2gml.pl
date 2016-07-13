:- module(
  rdf2gml,
  [
    rdf2gml/2, % +Source, +Sink
    rdf2gml/3  % +Source, +Sink, +Opts
  ]
).

/** <module> RDF GML

@author Wouter Beek
@version 2016/02-2016/04, 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(option)).
:- use_module(library(os/compress_ext)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/io)).
:- use_module(library(os/process_ext)).
:- use_module(library(pairs)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(thread)).
:- use_module(library(uuid)).

:- meta_predicate
    gml_label(4, +, -, +).





%! rdf2gml(+Source, +Sink) is det.
%! rdf2gml(+Source, +Sink, +Opts) is det.
%
% Source can be any source, but Sink must be a file.  The reason for
% this is that GML cannot be written in-stream because vertices and
% edges are stored separately.
%
% Options are passed to:
%
%   * call_onto_streams/7
%
%   * gml_edge/5
%
%   * gml_node/4
%
%   * rdf_call_on_tuples/3

rdf2gml(Source, Sink) :-
  rdf2gml(Source, Sink, _{}).


rdf2gml(Source, Sink, Opts) :-
  absolute_file_name(Sink, GFile, [access(write)]),
  uuid(Base),
  atomic_list_concat([Base,edges,tmp], ., EFile),
  atomic_list_concat([Base,nodes,tmp], ., NFile),
  call_cleanup(
    (
      call_onto_streams(Source, EFile, NFile, rdf2gml_stream(Opts), Opts, [], []),
      % Sort the nodes to ensure there are no duplicates.
      sort_file(NFile),
      % Concatenate the nodes and edges into one file.
      call_to_stream(GFile, writeln("graph [~n  directed 1")),
      format(atom(CatCmd), "cat ~a ~a >> ~a", [NFile,EFile,GFile]),
      run_process(sh, ['-c',CatCmd]),
      call_to_stream(GFile, writeln("]")),
      compress_file(GFile)
    ),
    concurrent_maplist(delete_file, [EFile,NFile])
  ).

rdf2gml_stream(Opts, In, Meta, Meta, NOut, EOut) :-
  rdf_call_on_tuples_stream(In, gml_triple(EOut, NOut, Opts), Meta, Opts).




%! gml_edge(+EOut, +NId1, +P, +NId2, +Opts) is det
%
% The following options are supported:
%
%   * edge_label_printer(+callable)

gml_edge(EOut, NId1, P, NId2, Opts) :-
  get_dict(edge_label_printer, Opts, Dcg_4, dcg_print_predicate),
  gml_label(Dcg_4, P, EL, Opts),
  format(EOut, "  edge [ label \"~a\" source ~a target ~a ]~n", [EL,NId1,NId2]).



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



%! gml_label(:Dcg_4, +T, -Label, +Opts) is det.

gml_label(Dcg_4, T, Lbl, Opts) :-
  phrase(dcg_call(Dcg_4, T, Opts), Cs1),
  phrase(gml_encode_label, Cs1, Cs2),
  string_codes(Lbl, Cs2).



%! gml_node(+NOut, +Opts, +N, -NId) is det.
%
% The following options are supported:
%
%   * node_label_printer(+callable)

gml_node(NOut, Opts, N, NId) :-
  get_dict(node_label_printer, Opts, Dcg_4, dcg_print_term),
  md5(N, NId),
  gml_label(Dcg_4, N, VL, Opts),
  format(NOut, "  node [ id ~a label \"~a\" ]~n", [NId,VL]).



%! gml_triple(+EOut, +NOut, +Opts, +Meta, +S, +P, +O, +G) is det.

gml_triple(EOut, NOut, Opts, _, S, P, O, _) :-
  maplist(gml_node(NOut, Opts), [S,O], [NId1,NId2]),
  gml_edge(EOut, NId1, P, NId2, Opts).



%! gml_unencoded(+Code) is semidet.
%
% ASCII characters, excluding double quote (34) and ampersand (38).

gml_unencoded(C) :-
  between(0,  33,  C), !.
gml_unencoded(C) :-
  between(35, 37,  C), !.
gml_unencoded(C) :-
  between(39, 127, C).
