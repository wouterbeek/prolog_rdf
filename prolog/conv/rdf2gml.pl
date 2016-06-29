:- module(
  rdf2gml,
  [
    rdf2gml/1, % +Source
    rdf2gml/2  % +Source, +BaseOut
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
:- use_module(library(os/open_any2)).
:- use_module(library(os/process_ext)).
:- use_module(library(pairs)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdfio)).

:- meta_predicate
    gml_cleanup(+, +, +, +, +, +, +, +),
    gml_label(4, +, -, +),
    gml_setup(+, -, -, -, -, -, -, -, -, -, +).





%! rdf2gml(+Source) is det.
%! rdf2gml(+Source, +Opts) is det.
%
% The following options are supported:
%
%   - out_base(+atom) The base name of the output files and the name
%   of the graph.
%
%   - Other options are passed to gml_setup/11 and gml_tuple/3.

rdf2gml(Source) :-
  rdf2gml(Source, _{}).


rdf2gml(Source, Opts) :-
  atomic_list_concat([DefBase|_], ., Source),
  get_dict(out_base, Opts, Base, DefBase),
  setup_call_cleanup(
    gml_setup(
      Base,
      NFile, NOut, NClose_0, NM,
      EFile, EOut, EClose_0, EM,
      GFile,
      Opts
    ),
    rdf_call_on_tuples(Source, gml_tuple(EOut, NOut, Opts)),
    gml_cleanup(Base, NFile, NClose_0, NM, EFile, EClose_0, EM, GFile)
  ).



%! gml_cleanup(
%!   +Base,
%!   +NFile, :NClose_0, NM,
%!   +EFile, :EClose_0, EM,
%!   +GFile
%! ) is det.

gml_cleanup(Base, NFile, NClose_0, NM1, EFile, EClose_0, EM1, GFile) :-
  close_any2(NClose_0, NM1, _),
  close_any2(EClose_0, EM1, _),

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
  compress_file(GFile),
  maplist(delete_file, [EFile,NFile]).



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



%! gml_setup(
%!   +Base,
%!   -NFile, -NOut, :NClose_0, -NM,
%!   -EFile, -EOut, :EClose_0, -EM,
%!   -GFile,
%!   +Opts
%! ) is det.
%
% The following options are supported:
%
%   * prefixes(+list(pair))

gml_setup(
  Base,
  NFile, NOut, NClose_0, NM,
  EFile, EOut, EClose_0, EM,
  GFile,
  Opts
) :-
  % Register prefixes.
  get_dict(prefixes, Opts, Pairs, []),
  pairs_keys_values(Pairs, Keys, Vals),
  maplist(rdf_register_prefix, Keys, Vals),

  atomic_list_concat([Base,gml,gz], ., GFile),
  atomic_list_concat([Base,edges,tmp], ., EFile),
  atomic_list_concat([Base,nodes,tmp], ., NFile),

  open_any2(EFile, write, EOut, EClose_0, EM),
  open_any2(NFile, write, NOut, NClose_0, NM).



%! gml_tuple(+EOut, +NOut, +Opts, +M, +S, +P, +O, +G) is det.

gml_tuple(EOut, NOut, Opts, _, S, P, O, _) :-
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
