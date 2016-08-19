:- module(
  q_cli,
  [
    q__cbd/1,   %     ?S
   %q__cbd/3,   % ?M, ?S,     ?HashG
   %q__cs/0,
   %q__cs/2,    % ?M,         ?HashG
    q__d/0,
    q__d/1,     % +HashG
    q__d/3,     % ?S, ?P, ?O
    q__d/4,     % ?S, ?P, ?O, ?HashG
    q__fs/0,
    q__fs/1,    % +HashG
    q__fs/2,    % +HashG, +PageOpts
    q__g/0,
    q__g/2,     % ?M,         ?HashG
    q__gs/0,
    q__key/3,   % ?M,     ?P, ?HashG
    q__p/1,     %         ?P
    q__p/3,     % ?M,     ?P, ?HashG
    q__p_ds/1,  %         ?P
    q__p_ds/3,  % ?M,     ?P, ?HashG
    q__p_os/1,  %         ?P
    q__p_os/3,  % ?M,     ?P, ?HashG
    q__p_ps/1,  %         ?P
    q__p_ps/3,  % ?M,     ?P, ?HashG
    q__ps/0,
    q__ps/2,    % ?M,         ?HashG
    q__ps_no/0,
    q__ps_no/2, % ?M,         ?HashG
    q__root/1,  %     ?S
   %q__root/3,  % ?M, ?S,     ?HashG
    q__scbd/1,  %     ?Node
   %q__scbd/3,  % ?M, ?Node,  ?HashG
    q__tree/1   %     ?S
   %q__tree/3   % ?M, ?S,     ?HashG
  ]
).
:- reexport(library(q/q_print), [
     q_print_cbd/3  as q__cbd,
     q_print_root/3 as q__root,
     q_print_scbd/3 as q__scbd,
     q_print_tree/3 as q__tree
   ]).

/** <module> Quine CLI

`HashG` is either a hash prefix or a graph term.

@author Wouter Beek
@version 2016/06-2016/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(pagination)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_graph)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(rdfs/rdfs_stat)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

:- rdf_meta
   q__cbd(r),
   q__cbd(+, r, r),
   q__cs(?, r),
   q__d(r, r, o),
   q__d(r, r, o, +),
   q__g(?, r),
   q__key(?, r, r),
   q__p(r),
   q__p(?, r, r),
   q__p_ds(r),
   q__p_ds(?, r, r),
   q__p_os(r),
   q__p_os(?, r, r),
   q__p_ps(r),
   q__p_ps(?, r, r),
   q__ps(?, r),
   q__ps_no(?, r),
   q__root(r),
   q__root(+, r, r),
   q__scbd(r),
   q__scbd(+, r, r),
   q__tree(r),
   q__tree(+, r, r).





%! q__cbd(?S) is nondet.

q__cbd(S) :-
  q__cbd(_, S, _).



%! q__cs is det.
%! q__cs(?M, ?HashG) is det.
%
% @tbd Add support for `M = hdt`.

% @tbd
%q__cs(trp, G) :-
%  findall(N-[C,pl(N)], (rdfs_class(C), rdfs_number_of_instances(C, N)), Pairs),
%  q_pairs_table0(["class","№ instances"], Pairs).



%! q__d is nondet.
%! q__d(+HashG) is nondet.
%! q__(?S, ?P, ?O) is nondet.
%! q__(?S, ?P, ?O, +HashG) is nondet.

q__d :-
  q__d('').


q__d(HashG) :-
  q__x(HashG, data).


q__d(S, P, O) :-
  q__d(S, P, O, '').


q__d(S, P, O, HashG) :-
  q__x(S, P, O, HashG, data).



%! q__fs is det.
%! q__fs(+HashG) is det.
%! q__fs(+HashG, +PageOpts) is det.

q__fs :-
  q__fs('').


q__fs(HashG) :-
  q__fs(HashG, _{}).


q__fs(HashG, PageOpts) :-
  pagination(Dir, q_dir(HashG, Dir), PageOpts, Result),
  pagination_result(Result, pp_hash_paths(HashG)).



%! q__g is nondet.
%! q__g(?M, ?HashG) is nondet.

q__g :-
  q_view_graph(M, G),
  q__g(M, G).


q__g(M, G) :-
  q_print_graph(M, G).



%! q__gs is det.

q__gs :-
  q_graph_table_comps(HeaderRow, DataRows),
  print_table([head(HeaderRow)|DataRows]).



%! q__key(?M, ?P, ?HashG) is nondet.

q__key(M, P, G) :-
  q_predicate(M, P, G),
  \+ ((
    q(M, S1, P, O, G),
    q(M, S2, P, O, G),
    S1 \== S2
  )),
  forall(q_subject(M, S, G), once(q(M, S, P, _, G))).



%! q__p(?P) is nondet.
%! q__p(?M, ?P, ?HashG) is nondet.

q__p(P) :-
  q__p(_, P, _).


q__p(M, P, G) :-
  q__p_os(M, P, G),
  q__p_ds(M, P, G).



%! q__p_ds(?P) is nondet.
%! q__p_ds(?M, ?P, ?HashG) is nondet.
%
% Shows the candidate datatypes for predicate term P.

q__p_ds(P) :-
  q_view_graph(M, G),
  q__p_ds(M, P, G).


q__p_ds(M, P, G) :-
  q_predicate(M, P, G),
  q_datatypes_compat(M, P, G, Ds),
  maplist(list_split, Rows, Ds),
  print_table([head([P])|Rows]).



%! q__p_os(?P) is nondet.
%! q__p_os(?M, ?P, ?HashG) is nondet.
%
% Prints an overview of how often each object term occurs.

q__p_os(P) :-
  q_view_graph(M, G),
  q__p_os(M, P, G).


q__p_os(M, P, G) :-
  q_predicate(M, P, G),
  (   \+ ((q(M, S1, P, O, G), q(M, S2, P, O, G), S1 \== S2))
  ->  q_p_no_abbr(M, P, G, "No reuse of object terms.")
  ;   aggregate_all(set(O), q(M, _, P, O, G), Os),
      (   length(Os, Len),
          Len > 5000
      ->  q_p_no_abbr(M, P, G, "Too many unique object terms.")
      ;   findall(
            N-[O,pl(N)],
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
%! q__p_ps(?M, ?P, ?HashG) is nondet.
%
% Predicate terms that are candidates for flattening.
%
% @see [[rdf_flatten/2]]

q__p_ps(P) :-
  q__p_ps(_, P, _).


q__p_ps(M, P, G) :-
  q_predicate(M, P, G),
  forall(q(M, _, P, O, G), q_is_bnode(O)),
  once(q(M, _, P, _, G)),
  aggregate_all(set(Q), (q(M, _, P, X, G), q(M, X, Q, _, G)), Qs),
  maplist(singleton_list, Qs, Rows),
  format(string(Lbl), "Next predicates of ~a", [P]),
  print_table([head([Lbl])|Rows]).



%! q__ps is det.
%! q__ps(?M, ?HashG) is det.

q__ps :-
  q_view_graph(M, G),
  q__ps(M, G).


q__ps(M, G) :-
  findall(
    N-[P,pl(N)],
    (
      distinct(P, q(M, _, P, _, G)),
      q_number_of_triples(M, _, P, _, G, N)
    ),
    Pairs
  ),
  q_pairs_table0([bold("predicate"),bold("№ occurrences")], Pairs).



%! q__ps_no is nondet.
%! q__ps_no(?M, ?HashG) is nondet.
%
% Prints an overview of how many distinct objects there are for a
% given predicate term.

q__ps_no :-
  q_view_graph(M, G),
  q__ps_no(M, G).


q__ps_no(M, G) :-
  findall(
    N-[P,pl(N)],
    (
      distinct(P, q_predicate(M, P, G)),
      q_number_of_objects(M, _, P, G, N)
    ),
    Pairs
  ),
  q_pairs_table0([bold("predicate"),bold("№ objects")], Pairs).



%! q__root(?S) is nondet.

q__root(S) :-
  q__root(_, S, _).



%! q__scbd(?Node) is nondet.

q__scbd(Node) :-
  q__scbd(_, Node, _).



%! q__tree(?S) is nondet.

q__tree(S) :-
  q__tree(_, S, _).





% HELPERS %

%! pp_files(+Dir) is det.
%
% Print the files in Dir.

pp_files(Dir) :-
  forall(
    directory_file(Dir, File),
    ansi_format([fg(green)], "    ~a", [File])
  ).



%! pp_hash_path(+HashG, +Path) is det.

pp_hash_path(HashG, Path) :-
  format(current_output, "  ", []),
  q_dir_hash(Path, Hash),
  atom_concat(HashG, Rest, Hash),
  directory_file_path(Path, done, Done),
  (exists_file(Done) -> Color = green ; Color = red),
  ansi_format(current_output, [fg(Color)], "~a|~a:  ", [HashG,Rest]),
  pp_files(Path),
  nl.



%! pp_hash_paths(+HashG, +Paths) is det.

pp_hash_paths(HashG, Paths) :-
  maplist(pp_hash_path(HashG), Paths).



%! q__x(+HashG, +Name) is nondet.
%! q__x(?S, ?P, ?O, +HashG, +Name) is nondet.

q__x(HashG, Name) :-
  q__x(_, _, _, HashG, Name).


q__x(S, P, O, HashG, Name) :-
  pagination(rdf(S,P,O), q__x0(S, P, O, HashG, Name), Result),
  pagination_result(Result, q_print_quads).


q__x0(S, P, O, HashG, Name) :-
  q_graph(HashG, Name, G),
  hdt_call_graph(G, hdt0(S, P, O)).



%! q_p_no_abbr(+M, +P, +G, +Msg) is det.

q_p_no_abbr(M, P, G, Msg) :-
  ansi_format(user_output, [fg(yellow)], "~s~n", [Msg]),
  once(findnsols(5, O, distinct(O, q(M, _, P, O, G)), Os)),
  maplist(singleton_list, Os, Rows),
  print_table([head([bold("object")])|Rows]).



%! q_pairs_table0(+HeaderRow, +Pairs) is det.

q_pairs_table0(HeaderRow, Pairs) :-
  asc_pairs(Pairs, SortedPairs),
  pairs_values(SortedPairs, DataRows),
  print_table([head(HeaderRow)|DataRows]).
