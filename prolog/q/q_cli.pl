:- module(
  q_cli,
  [
    q__cbd/1,   % ?S
    q__cbd/2,   % ?S, ?G
    q__ds/0,
    q__fs/0,
    q__fs/1,    % +PrefixHash
    q__fs/2,    % +PrefixHash, +PageOpts
    q__g/0,
    q__g/1,     % ?G
    q__gs/0,
    q__io/0,
    q__key/1,   % ?P
    q__key/2,   % ?P, ?G
    q__link/3,  % +Backend, +P, +G
    q__load/1,  % +File
    q__m/0,
    q__m/1,     % ?G
    q__m/3,     % ?S, ?P, ?O
    q__m/4,     % ?S, ?P, ?O, ?G
    q__p/0,
    q__p/1,     % ?P
    q__p/2,     % ?P, ?G
    q__p_ds/1,  % ?P
    q__p_ds/2,  % ?P, ?G
    q__p_os/1,  % ?P
    q__p_os/2,  % ?P, ?G
    q__p_ps/1,  % ?P
    q__p_ps/2,  % ?P, ?G
    q__ps/0,
    q__ps/1,    % ?G
    q__ps_no/0,
    q__ps_no/1, % ?G
    q__root/1,  % ?S
    q__root/2,  % ?S, ?G
    q__scbd/1,  % ?Node
    q__scbd/2,  % ?Node, ?G
    q__tree/1,  % ?S
    q__tree/2,  % ?S, ?G
    q__w/0,
    q__w/1,     % ?G
    q__w/3,     % ?S, ?P, ?O
    q__w/4,     % ?S, ?P, ?O, ?G
    q__x/1,     % ?Base
    q__x/2,     % ?Base, ?G
    q__x/4,     % ?Base, ?S, ?P, ?O
    q__x/5      % ?Base, ?S, ?P, ?O, +G
  ]
).

/** <module> Quine CLI

`G` is either a hash prefix or a graph term.

@author Wouter Beek
@tbd q__cs/[0,1] for enumerating classes.
@version 2016/06-2016/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_cli)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(pagination/cli_pagination)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_dataset)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_graph)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_link)).
:- use_module(library(q/q_prefix), []).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).
:- use_module(library(tree/s_tree)).
:- use_module(library(yall)).

:- meta_predicate
    q__io(+, 1).
    
:- rdf_meta
   q__cbd(r),
   q__cbd(r, r),
   q__g(r),
   q__key(r),
   q__key(r, r),
   q__link(+, r, r),
   q__m(r),
   q__m(r, r, o),
   q__m(r, r, o, r),
   q__p(r),
   q__p(r, r),
   q__p_ds(r),
   q__p_ds(r, r),
   q__p_os(r),
   q__p_os(r, r),
   q__p_ps(r),
   q__p_ps(r, r),
   q__ps(r),
   q__ps_no(r),
   q__root(r),
   q__root(r, r),
   q__scbd(r),
   q__scbd(r, r),
   q__tree(r),
   q__tree(r, r),
   q__w(r),
   q__w(r, r, o),
   q__w(r, r, o, r),
   q__x(+, r),
   q__x(+, r, r, o),
   q__x(+, r, r, o, r).





%! q__cbd(?S) is nondet.
%! q__cbd(?S, ?G) is nondet.

q__cbd(S) :-
  q__cbd(S, _).


q__cbd(S, G) :-
  q_store_graph(data, G),
  q_subject(hdt, S, G),
  q_print_cbd(hdt, S, G).



%! q__ds is det.

q__ds :-
  aggregate_all(
    set(q_dataset_term(D)-q_graph_term(G)),
    q_dataset_graph(D, G),
    Pairs
  ),
  (pairs_to_tree(Pairs, Tree) -> print_tree(Tree) ; writeln("∅")).



%! q__fs is det.
%! q__fs(+PrefixHash) is det.
%! q__fs(+PrefixHash, +PageOpts) is det.

q__fs :-
  q__fs('').


q__fs(PrefixHash) :-
  q__fs(PrefixHash, _{}).


q__fs(PrefixHash, PageOpts) :-
  create_pagination(Dir, q_dir(PrefixHash, Dir), PageOpts, Pagination),
  cli_pagination_result(Pagination, pp_hash_paths(PrefixHash)).



%! q__g is nondet.
%! q__g(?G) is nondet.

q__g :-
  q__g(_).


q__g(G) :-
  q_store_graph(data, G),
  q_print_graph(hdt, G).



%! q__gs is det.

q__gs :-
  q_graph_table_comps(HeaderRow, DataRows),
  print_table([head(HeaderRow)|DataRows]).



%! q__io is det.

q__io :-
  q_source_dir(Dir1),
  q__io(source(Dir1), q_source_file),
  q_store_dir(Dir2),
  q__io(store(Dir2), q_store_graph),
  forall(
    q_backend(M),
    q__io(cache(M), q_cache_graph(M))
  ),
  forall(
    q_backend(M),
    q__io(view(M), q_view_graph(M))
  ).

q__io(Root, Goal_1) :-
  aggregate_all(set(Root-G), call(Goal_1, G), Pairs),
  (   pairs_to_tree(Pairs, Tree)
  ->  print_tree(Tree, [label_writer(q_cli:print_io_term)])
  ;   writeln("∅")
  ).

print_io_term(G) -->
  {q_graph_hash(G, Hash)}, !,
  atom(Hash).
print_io_term(Term) -->
  atom(Term).



%! q__key(?P) is nondet.
%! q__key(?P, ?G) is nondet.

q__key(P) :-
  q__key(P, _).


q__key(P, G) :-
  q_store_graph(data, G),
  q_predicate(hdt, P, G),
  \+ ((
    q(hdt, S1, P, O, G),
    q(hdt, S2, P, O, G),
    S1 \== S2
  )),
  forall(q_subject(hdt, S, G), once(q(hdt, S, P, _, G))).



%! q__link(+Backend, +P, +G) is det.

q__link(Backend, P, G) :-
  q_link_objects(Backend, P, G).
  


%! q__load(+File) is det.

q__load(SourceFile) :-
  q_source2store_file(SourceFile, StoreFile),
  q_store_file(StoreFile, data, G),
  q_store2view(hdt, G),
  msg_notification("‘~a’ → ‘~a’~n", [SourceFile,StoreFile]).



%! q__m is nondet.
%! q__m(?G) is nondet.
%! q__m(?S, ?P, ?O) is nondet.
%! q__m(?S, ?P, ?O, ?G) is nondet.

q__m :-
  q__m(_).


q__m(G) :-
  q__m(_, _, _, G).


q__m(S, P, O) :-
  q__m(S, P, O, _).


q__m(S, P, O, G) :-
  q__x(meta, S, P, O, G).



%! q__p is nondet.
%! q__p(?P) is nondet.
%! q__p(?P, ?G) is nondet.

q__p :-
  q__p(_).


q__p(P) :-
  q__p(P, _).


q__p(P, G) :-
  q_store_graph(data, G),
  q_predicate(hdt, P, G),
  dcg_with_output_to(
    section((
      "Predicate ",
      sq(dcg_q_print_predicate(P)),
      " in graph ",
      sq(dcg_q_print_graph_term(G))
    ))
  ),
  q__p_os0(hdt, P, G),
  q__p_ds0(hdt, P, G).



%! q__p_ds(?P) is nondet.
%! q__p_ds(?P, ?G) is nondet.
%
% Shows the candidate datatypes for predicate term P.

q__p_ds(P) :-
  q__p_ds(P, _).


q__p_ds(P, G) :-
  q_store_graph(data, G),
  q_predicate(hdt, P, G),
  q__p_ds0(hdt, P, G).


q__p_ds0(hdt, P, G) :-
  q_datatypes_compat(hdt, P, G, Ds),
  maplist(list_split, Rows, Ds),
  print_table([head([q_predicate(P)])|Rows]).



%! q__p_os(?P) is nondet.
%! q__p_os(?P, ?G) is nondet.
%
% Prints an overview of how often each object term occurs.

q__p_os(P) :-
  q__p_os(P, _).


q__p_os(P, G) :-
  q_store_graph(data, G),
  q_predicate(hdt, P, G),
  q__p_os0(P, G).


q__p_os0(P, G) :-
  (   \+ ((q(hdt, S1, P, O, G), q(hdt, S2, P, O, G), S1 \== S2))
  ->  q_p_no_abbr(P, G, "No reuse of object terms.")
  ;   aggregate_all(set(O), q(hdt, _, P, O, G), Os),
      (   length(Os, Len),
          Len > 5000
      ->  q_p_no_abbr(P, G, "Too many unique object terms.")
      ;   findall(
            NumSs-[q_object(O),thousands(NumSs)],
            (
              member(O, Os),
              q_number_of_subjects(hdt, P, O, G, NumSs)
            ),
            Pairs
          ),
          q_pairs_table0([string("object"),string("№ occurrences")], Pairs)
      )
  ).



%! q__p_ps(?P) is nondet.
%! q__p_ps(?P, ?G) is nondet.
%
% Predicate terms that are candidates for flattening.
%
% @see [[rdf_flatten/2]]

q__p_ps(P) :-
  q__p_ps(P, _).


q__p_ps(P, G) :-
  q_store_graph(data, G),
  q_predicate(hdt, P, G),
  forall(q(hdt, _, P, O, G), q_is_bnode(O)),
  once(q(hdt, _, P, _, G)),
  aggregate_all(set(Q), (q(hdt, _, P, X, G), q(hdt, X, Q, _, G)), Qs),
  maplist(singleton_list, Qs, Rows),
  format(string(Lbl), "Next predicates of ~a", [P]),
  print_table([head([Lbl])|Rows]).



%! q__ps is det.
%! q__ps(?G) is det.

q__ps :-
  q__ps(_).


q__ps(G) :-
  q_store_graph(data, G),
  dcg_with_output_to(section(("Graph ",dcg_q_print_graph_term(G)))),
  findall(
    N-[q_predicate(P),thousands(N)],
    (
      distinct(P, q(hdt, _, P, _, G)),
      q_number_of_triples(hdt, _, P, _, G, N)
    ),
    Pairs
  ),
  q_pairs_table0([string("predicate"),string("№ occurrences")], Pairs).



%! q__ps_no is nondet.
%! q__ps_no(?G) is nondet.
%
% Prints an overview of how many distinct objects there are for a
% given predicate term.

q__ps_no :-
  q__ps_no(_).


q__ps_no(G) :-
  q_store_graph(data, G),
  findall(
    N-[q_predicate(P),thousands(N)],
    (
      distinct(P, q_predicate(hdt, P, G)),
      q_number_of_objects(hdt, _, P, G, N)
    ),
    Pairs
  ),
  q_pairs_table0([string("predicate"),string("№ objects")], Pairs).



%! q__root(?S) is nondet.
%! q__root(?S, ?G) is nondet.

q__root(S) :-
  q__root(S, _).


q__root(S, G) :-
  q_store_graph(data, G),
  q_subject(hdt, S, G),
  q_print_root(hdt, S, G).



%! q__scbd(?Node) is nondet.
%! q__scbd(?Node, ?G) is nondet.

q__scbd(Node) :-
  q__scbd(Node, _).


q__scbd(Node, G) :-
  q_store_graph(data, G),
  q_node(hdt, Node, G),
  q_print_scbd(hdt, Node, G).



%! q__tree(?S) is nondet.
%! q__tree(?S, ?G) is nondet.

q__tree(S) :-
  q__tree(S, _).


q__tree(S, G) :-
  q_store_graph(data, G),
  q_subject(hdt, S, G),
  q_print_tree(hdt, S, G).



%! q__w is nondet.
%! q__w(?G) is nondet.
%! q__w(?S, ?P, ?O) is nondet.
%! q__w(?S, ?P, ?O, ?G) is nondet.

q__w :-
  q__w(_).


q__w(G) :-
  q__w(_, _, _, G).


q__w(S, P, O) :-
  q__w(S, P, O, _).


q__w(S, P, O, G) :-
  q__x(warn, S, P, O, G).



%! q__x(?Base) is nondet.
%! q__x(?Base, ?G) is nondet.
%! q__x(?Base, ?S, ?P, ?O) is nondet.
%! q__x(?Base, ?S, ?P, ?O, ?G) is nondet.

q__x(Base) :-
  q__x(Base, _).


q__x(Base, G) :-
  q__x(Base, _, _, _, G).


q__x(Base, S, P, O) :-
  q__x(Base, S, P, O, _).


q__x(Base, S, P, O, G) :-
  create_pagination(
    rdf(S,P,O),
    (
      q_store_graph(Base, G),
      q(hdt, S, P, O, G)
    ),
    Result
  ),
  cli_pagination_result(Result, q_print_quads).





% HELPERS %

%! pp_files(+Dir) is det.
%
% Print the files in Dir.

pp_files(Dir) :-
  forall(
    directory_file(Dir, File),
    ansi_format([fg(green)], "    ~a", [File])
  ).



%! pp_hash_path(+Prefix, +Path) is det.

pp_hash_path(Prefix, Path) :-
  format(current_output, "  ", []),
  q_dir_hash(Path, Hash),
  atom_concat(Prefix, Rest, Hash),
  directory_file_path(Path, done, Done),
  (exists_file(Done) -> Color = green ; Color = red),
  ansi_format(current_output, [fg(Color)], "~a|~a:  ", [Prefix,Rest]),
  pp_files(Path),
  nl.



%! pp_hash_paths(+Prefix, +Paths) is det.

pp_hash_paths(Prefix, Paths) :-
  maplist(pp_hash_path(Prefix), Paths).



%! q_p_no_abbr(+P, +G, +Msg) is det.

q_p_no_abbr(P, G, Msg) :-
  ansi_format(user_output, [fg(yellow)], "~s~n", [Msg]),
  once(findnsols(5, O, distinct(O, q(hdt, _, P, O, G)), Os)),
  maplist(object_row, Os, Rows),
  print_table([head([string("object")])|Rows]).

object_row(O, [q_object(O)]).



%! q_pairs_table0(+HeaderRow, +Pairs) is det.

q_pairs_table0(HeaderRow, Pairs) :-
  Opts = [],
  group_pairs_by_key(Pairs, Groups1),
  maplist(call_on_value(sort), Groups1, Groups2),
  desc_pairs_values(Groups2, SortedGroups),
  append(SortedGroups, DataRows),
  option(max_number_of_rows(MaxNumRows), Opts, inf),
  list_truncate(DataRows, MaxNumRows, DataRows0),
  print_table([head(HeaderRow)|DataRows0]).
