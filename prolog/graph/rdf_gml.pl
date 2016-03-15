:- module(
  rdf_gml,
  [
    rdf_load_gml/1, % +Source
    rdf_load_gml/2  % +Source, +BaseOut
  ]
).

/** <module> RDF GML

@author Wouter Beek
@version 2016/02-2016/03
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(option)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/open_any2)).
:- use_module(library(os/process_ext)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    gml_cleanup(+, +, +, +, +, +),
    gml_label(4, +, -, +),
    gml_setup(+, -, -, -, -, -, -, -, +).

:- predicate_options(gml_edge/5, 5, [
     edge_label_writer(+callable),
     node_label_writer(+callable),
     out_base(+atom),
     prefixes(+list(pair)),
     pass_to(rdf_print_predicate//2, 2),
     pass_to(rdf_print_term//2, 2)
   ]).





%! rdf_load_gml(+Source) is det.
% Wrapper around rdf_load_gml/2 with default options.

rdf_load_gml(Source) :-
  rdf_load_gml(Source, []).


%! rdf_load_gml(+Source, +Opts) is det.

rdf_load_gml(Source, Opts) :-
  option(out_base(Base), Opts, out),
  setup_call_cleanup(
    gml_setup(
      Base,
      NFile, NOut, NClose_0,
      EFile, EOut, EClose_0,
      GFile,
      Opts
    ),
    rdf_call_on_tuples(Source, gml_tuples(EOut, NOut, Opts)),
    gml_cleanup(Base, NFile, NClose_0, EFile, EClose_0, GFile)
  ).



%! gml_cleanup(
%!   +Base,
%!   +NFile, :NClose_0,
%!   +EFile, :EClose_0,
%!   +GFile
%! ) is det.

gml_cleanup(Base, NFile, NClose_0, EFile, EClose_0, GFile) :-
  close_any2(NClose_0),
  close_any2(EClose_0),

  % Sort the nodes to ensure there are no duplicates.
  sort_file(NFile),

  % Concatenate the nodes and edges into one file.
  setup_call_cleanup(
    open(GFile, write, In2),
    format(In2, "graph [~n  comment \"~a\"~n  directed 1~n", [Base]),
    close(In2)
  ),
  format(atom(CatCmd), "cat ~a ~a >> ~a", [NFile,EFile,GFile]),
  run_process(sh, ['-c',CatCmd]),
  setup_call_cleanup(
    open(GFile, append, In3),
    format(In3, "]~n", []),
    close(In3)
  ),
  run_process(gzip, [file(GFile)]),
  maplist(delete_file, [EFile,NFile]).



%! gml_edge(+EOut, +NId1, +P, +NId2, +Opts) is det
% The following options are supported:
%   - edge_label_printer(+callable)

gml_edge(EOut, NId1, P, NId2, Opts) :-
  option(edge_label_printer(Dcg_2), Opts, rdf_print_predicate),
  gml_label(Dcg_2, P, EL, Opts),
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



%! gml_label(:Dcg_2, +T, -Label, +Opts) is det.

gml_label(Dcg_2, T, Lbl, Opts) :-
  phrase(dcg_call(Dcg_2, T, Opts), Cs1),
  phrase(gml_encode_label, Cs1, Cs2),
  string_codes(Lbl, Cs2).



%! gml_node(+NOut, +Opts, +N, -NId) is det.
% The following options are supported:
%   - node_label_printer(+callable)

gml_node(NOut, Opts, N, NId) :-
  option(node_label_printer(Dcg_2), Opts, rdf_print_term),
  md5(N, NId),
  gml_label(Dcg_2, N, VL, Opts),
  format(NOut, "  node [ id ~a label \"~a\" ]~n", [NId,VL]).



%! gml_setup(
%!   +Base,
%!   -NFile, -NOut, :NClose_0,
%!   -EFile, -EOut, :EClose_0,
%!   -GFile,
%!   +Opts
%! ) is det.
% The following options are supported:
%   - out_base(+atom)
%     The base name of the output files and the name of the graph.
%   - prefixes(+list(pair))

gml_setup(
  Base,
  NFile, NOut, NClose_0,
  EFile, EOut, EClose_0,
  GFile,
  Opts
) :-
  % Register prefixes.
  option(prefixes(Pairs), Opts, []),
  pairs_keys_values(Pairs, Keys, Vals),
  maplist(rdf_register_prefix, Keys, Vals),

  atomic_list_concat([Base,gml], ., GFile),
  atomic_list_concat([Base,edges,tmp], ., EFile),
  atomic_list_concat([Base,nodes,tmp], ., NFile),

  open_any2(EFile, write, EOut, EClose_0),
  open_any2(NFile, write, NOut, NClose_0).



%! gml_tuple(+EOut, +NOut, +Opts, +Stmt) is det.

gml_tuple(EOut, NOut, Opts, Stmt) :-
  call_collect_messages((
    (Stmt = rdf(S,P,O0), ! ; Stmt = rdf(S,P,O0,_)),
    rdf11:post_object(O, O0),
    gml_triple(EOut, NOut, Opts, S, P, O)
  )).



%! gml_tuples(+EOut, +NOut, +Opts, +Tuples, ?G) is det.

gml_tuples(EOut, NOut, Opts, Tuples, _) :-
  maplist(gml_tuple(EOut, NOut, Opts), Tuples).



%! gml_triple(+EOut, +NOut, +Opts, +S, +P, +O) is det.

gml_triple(EOut, NOut, Opts, S, P, O) :-
  maplist(gml_node(NOut, Opts), [S,O], [NId1,NId2]),
  gml_edge(EOut, NId1, P, NId2, Opts).



%! gml_unencoded(+Code) is semidet.
% ASCII characters, excluding double quote (34) and ampersand (38).

gml_unencoded(C) :- between(0,  33,  C), !.
gml_unencoded(C) :- between(35, 37,  C), !.
gml_unencoded(C) :- between(39, 127, C).
