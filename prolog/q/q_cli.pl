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
    q__x/0,
    q__x/1,     % ?G
    q__x/3,     % ?S, ?P, ?O
    q__x/4      % ?S, ?P, ?O, +G
  ]
).

/** <module> Quine CLI

`G` is either a hash prefix or a graph term.

@author Wouter Beek
@tbd q__cs/[0,1] for enumerating classes.
@version 2016/06-2016/08, 2016/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_cli)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(pagination)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_dataset_db)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_graph)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_prefix), []).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(rdfs/rdfs_stat)).
:- use_module(library(semweb/rdf11)).
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
   q__x(r),
   q__x(r, r, o),
   q__x(r, r, o, r).

:- setting(
     backend,
     oneof([hdt,trp]),
     hdt,
     "The backend that is used to power the Quine CLI."
   ).





%! q__cbd(?S) is nondet.
%! q__cbd(?S, ?G) is nondet.

q__cbd(S) :-
  q__cbd(S, _).


q__cbd(S, G) :-
  setting(backend, M),
  q_subject(M, S, G),
  q_print_cbd(M, S, G).



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
  pagination(Dir, q_dir(PrefixHash, Dir), PageOpts, Pagination),
  pagination_result(Pagination, pp_hash_paths(PrefixHash)).



%! q__g is nondet.
%! q__g(?G) is nondet.

q__g :-
  q__g(_).


q__g(G) :-
  setting(backend, M),
  q_view_graph(M, G),
  q_print_graph(M, G).



%! q__gs is det.

q__gs :-
  q_graph_table_comps(HeaderRow, DataRows),
  print_table([head(HeaderRow)|DataRows]).



%! q__io is det.

q__io :-
  setting(source_dir, Dir1),
  q__io(source(Dir1), q_source_file),
  setting(store_dir, Dir2),
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
  setting(backend, M),
  q_predicate(M, P, G),
  \+ ((
    q(M, S1, P, O, G),
    q(M, S2, P, O, G),
    S1 \== S2
  )),
  forall(q_subject(M, S, G), once(q(M, S, P, _, G))).



%! q__p is nondet.
%! q__p(?P) is nondet.
%! q__p(?P, ?G) is nondet.

q__p :-
  q__p(_).


q__p(P) :-
  q__p(P, _).


q__p(P, G) :-
  setting(backend, M),
  q_predicate(M, P, G),
  dcg_with_output_to(
    section((
      "Predicate ",
      dcg_q_print_predicate(P),
      " in graph ",
      dcg_q_print_graph_term(G)
    ))
  ),
  q__p_os0(M, P, G),
  q__p_ds0(M, P, G).



%! q__p_ds(?P) is nondet.
%! q__p_ds(?P, ?G) is nondet.
%
% Shows the candidate datatypes for predicate term P.

q__p_ds(P) :-
  q__p_ds(P, _).


q__p_ds(P, G) :-
  setting(backend, M),
  q_predicate(M, P, G),
  q__p_ds0(M, P, G).


q__p_ds0(M, P, G) :-
  q_datatypes_compat(M, P, G, Ds),
  maplist(list_split, Rows, Ds),
  print_table([head([q_predicate(P)])|Rows]).



%! q__p_os(?P) is nondet.
%! q__p_os(?P, ?G) is nondet.
%
% Prints an overview of how often each object term occurs.

q__p_os(P) :-
  q__p_os(P, _).


q__p_os(P, G) :-
  setting(backend, M),
  q_predicate(M, P, G),
  q__p_os0(M, P, G).


q__p_os0(M, P, G) :-
  (   \+ ((q(M, S1, P, O, G), q(M, S2, P, O, G), S1 \== S2))
  ->  q_p_no_abbr(M, P, G, "No reuse of object terms.")
  ;   aggregate_all(set(O), q(M, _, P, O, G), Os),
      (   length(Os, Len),
          Len > 5000
      ->  q_p_no_abbr(M, P, G, "Too many unique object terms.")
      ;   findall(
            N-[q_object(O),thousands(N)],
            (
              member(O, Os),
              q_number_of_subjects(M, P, O, G, N)
            ),
            Pairs
          ),
          q_pairs_table0([bold("object"),bold("№ occurrences")], Pairs)
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
  setting(backend, M),
  q_predicate(M, P, G),
  forall(q(M, _, P, O, G), q_is_bnode(O)),
  once(q(M, _, P, _, G)),
  aggregate_all(set(Q), (q(M, _, P, X, G), q(M, X, Q, _, G)), Qs),
  maplist(singleton_list, Qs, Rows),
  format(string(Lbl), "Next predicates of ~a", [P]),
  print_table([head([Lbl])|Rows]).



%! q__ps is det.
%! q__ps(?G) is det.

q__ps :-
  q__ps(_).


q__ps(G) :-
  setting(backend, M),
  q_view_graph(M, G),
  dcg_with_output_to(
    section((
      "Graph ",
      dcg_q_print_graph_term(G),
      " in backend ",
      atom(M)
    ))
  ),
  findall(
    N-[q_predicate(P),thousands(N)],
    (
      distinct(P, q(M, _, P, _, G)),
      q_number_of_triples(M, _, P, _, G, N)
    ),
    Pairs
  ),
  q_pairs_table0([bold("predicate"),bold("№ occurrences")], Pairs).



%! q__ps_no is nondet.
%! q__ps_no(?G) is nondet.
%
% Prints an overview of how many distinct objects there are for a
% given predicate term.

q__ps_no :-
  q__ps_no(_).


q__ps_no(G) :-
  setting(backend, M),
  q_view_graph(M, G),
  findall(
    N-[q_predicate(P),thousands(N)],
    (
      distinct(P, q_predicate(M, P, G)),
      q_number_of_objects(M, _, P, G, N)
    ),
    Pairs
  ),
  q_pairs_table0([bold("predicate"),bold("№ objects")], Pairs).



%! q__root(?S) is nondet.
%! q__root(?S, ?G) is nondet.

q__root(S) :-
  q__root(S, _).


q__root(S, G) :-
  setting(backend, M),
  q_subject(M, S, G),
  q_print_root(M, S, G).



%! q__scbd(?Node) is nondet.
%! q__scbd(?Node, ?G) is nondet.

q__scbd(Node) :-
  q__scbd(Node, _).


q__scbd(Node, G) :-
  setting(backend, M),
  q_node(M, Node, G),
  q_print_scbd(M, Node, G).



%! q__tree(?S) is nondet.
%! q__tree(?S, ?G) is nondet.

q__tree(S) :-
  q__tree(S, _).


q__tree(S, G) :-
  setting(backend, M),
  q_subject(M, S, G),
  q_print_tree(M, S, G).



%! q__x is nondet.
%! q__x(?G) is nondet.
%! q__x(?S, ?P, ?O) is nondet.
%! q__x(?S, ?P, ?O, ?G) is nondet.

q__x :-
  q__x(_).


q__x(G) :-
  q__x(_, _, _, G).


q__x(S, P, O) :-
  q__x(S, P, O, _).


q__x(S, P, O, G) :-
  setting(backend, M),
  pagination(rdf(S,P,O), q(M, S, P, O, G), Result),
  pagination_result(Result, q_print_quads).





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



%! q_p_no_abbr(+M, +P, +G, +Msg) is det.

q_p_no_abbr(M, P, G, Msg) :-
  ansi_format(user_output, [fg(yellow)], "~s~n", [Msg]),
  once(findnsols(5, O, distinct(O, q(M, _, P, O, G)), Os)),
  maplist(object_row, Os, Rows),
  print_table([head([bold("object")])|Rows]).

object_row(O, [q_object(O)]).



%! q_pairs_table0(+HeaderRow, +Pairs) is det.

q_pairs_table0(HeaderRow, Pairs) :-
  group_pairs_by_key(Pairs, Groups1),
  maplist(call_on_value(sort), Groups1, Groups2),
  asc_pairs_values(Groups2, SortedGroups),
  append(SortedGroups, DataRows),
  print_table([head(HeaderRow)|DataRows]).
