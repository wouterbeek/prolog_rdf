:- module(
  rc,
  [
    cand_datatype/1, %     ?P
    cand_datatype/3, % ?M, ?P, ?G
    cand_flatten/0,
    cand_flatten/2,  % ?M,     ?G
    cand_flatten/3,  % ?M, ?P, ?G
    rc_classes/0,
    rc_graph/0,
    rc_graph/2,      % ?M,     ?G
    rc_graphs/0,
    rc_graphs/1,     % ?M
    rc_p/1,          %     ?P
    rc_p/3,          % ?M, ?P, ?G
    rc_p_no/0,
    rc_p_no/1,       %     ?P
    rc_p_no/2,       % ?M,     ?G
    rc_p_no/3,       % ?M, ?P, ?G
    rc_predicates/0,
    rc_predicates/2  % ?M,     ?G
  ]
).
:- reexport(library(z/z_print), [
     z_print_cbd/1  as rc_cbd,
     z_print_cbd/2  as rc_cbd,
     z_print_root/1 as rc_root,
     z_print_root/2 as rc_root,
     z_print_scbd/1 as rc_scbd,
     z_print_scbd/2 as rc_scbd,
     z_print_tree/1 as rc_tree,
     z_print_tree/2 as rc_tree
   ]).

/** <module> RDF CLI

@author Wouter Beek
@version 2016/05-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(rdfs/rdfs_stat)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(stat/r_ext)).
:- use_module(library(yall)).
:- use_module(library(z/z_cbd)).
:- use_module(library(z/z_datatype)).
:- use_module(library(z/z_ext)).
:- use_module(library(z/z_print)).
:- use_module(library(z/z_shape)).
:- use_module(library(z/z_stat)).
:- use_module(library(z/z_stmt)).
:- use_module(library(z/z_term)).

:- rdf_meta
   cand_datatype(r),
   cand_datatype(?, r, r),
   cand_flatten(?, r),
   cand_flatten(?, r, r),
   rc_graph(?, r),
   rc_p(r),
   rc_p(?, r, r),
   rc_p_no(r),
   rc_p_no(?, r),
   rc_p_no(?, r, r),
   rc_predicates(?, r).





%! cand_datatype(?P) is nondet.
%! cand_datatype(?M, ?P, ?G) is nondet.
%
% Shows the candidate datatypes per predicate term.

cand_datatype(P) :-
  z_graph(M, G),
  cand_datatype(M, P, G).


cand_datatype(M, P, G) :-
  z_predicate(M, P, G),
  z_datatypes_compat(M, P, G, Ds),
  maplist(list_split, Rows, Ds),
  z_print_table([head([P])|Rows]).



%! cand_flatten is nondet.
%! cand_flatten(?M, ?G) is nondet.
%! cand_flatten(?M, ?P, ?G) is nondet.
%
% Candidates for [[rdf_flatten/2]].

cand_flatten :-
  z_graph(M, G),
  cand_flatten(M, G).


cand_flatten(M, G) :-
  findall(
    [P,N],
    (cand_flatten(M, P, G), z_number_of_triples(M, _, P, _, G, N)),
    Rows
  ),
  z_print_table([head(["Predicate","#occurrences"])|Rows]).


cand_flatten(M, P, G) :-
  z_predicate(M, P, G),
  forall(z(M, _, P, O, G), rdf_is_bnode(O)),
  once(z(M, _, P, _, G)),
  aggregate_all(set(Q), (z(M, _, P, X, G), z(M, X, Q, _, G)), Qs),
  maplist(singleton_list, Qs, Rows),
  format(string(Lbl), "Next predicates of ~a", [P]),
  z_print_table([head([Lbl])|Rows]).



%! rc_classes is det.
% @tbd

rc_classes :-
  findall(N-C, (rdfs_class(C), rdfs_number_of_instances(C, N)), Pairs),
  z_pairs_table0(["Class","#instances"], Pairs).



%! rc_graph is nondet.
%! rc_graph(?M, ?G) is nondet.

rc_graph :-
  z_graph(M, G),
  rc_graph(M, G).


rc_graph(M, G) :-
  z_print_graph(M, G).



%! rc_graphs is det.
%! rc_graphs(?M) is det.

rc_graphs :-
  rc_graphs(_).


rc_graphs(M) :-
  findall(N-G, z_number_of_triples(M, G, N), Pairs),
  z_pairs_table0(["Graph","#triples"], Pairs).



%! rc_p(?P) is nondet.
%! rc_p(?M, ?P, ?G) is nondet.

rc_p(P) :-
  z_graph(M, G),
  rc_p(M, P, G).


rc_p(M, P, G) :-
  rc_p_no(M, P, G),
  cand_datatype(M, P, G).



%! rc_p_no is nondet.
%! rc_p_no(?M, ?G) is nondet.
%
% Prints an overview of how many distinct objects there are for a
% given predicate term.

rc_p_no :-
  z_graph(M, G),
  rc_p_no(M, G).


rc_p_no(M, G) :-
  aggregate_all(set(P), z_predicate(M, P, G), Ps),
  maplist({M,G}/[P,N]>>z_number_of_objects(M, _, P, G, N), Ps, Ns),
  rdf_counts_resources_table0(["Predicate","#objects"], Ns, Ps).


%! rc_p_no(?P) is nondet.
%! rc_p_no(?M, ?P, ?G) is nondet.
%
% Prints an overview of how often each object term occurs.

rc_p_no(P) :-
  z_graph(M, G),
  rc_p_no(M, P, G).


rc_p_no(M, P, G) :-
  z_predicate(M, P, G),
  (   \+ ((z(M, S1, P, O, G), z(M, S2, P, O, G), S1 \== S2))
  ->  rc_p_no_abbr(M, P, G, "No reuse of object terms.")
  ;   aggregate_all(set(O), z(M, _, P, O, G), Os),
      (   length(Os, Len),
          Len > 1000
      ->  rc_p_no_abbr(M, P, G, "Too many unique object terms.")
      ;   maplist({M,P,G}/[O,N]>>z_number_of_subjects(M, P, O, G, N), Os, Ns),
          rdf_counts_resources_table0(["Object","#occurrences"], Ns, Os)
      )
  ).


rc_p_no_abbr(M, P, G, Msg) :-
  ansi_format(user_output, [fg(yellow)], "~s~n", [Msg]),
  once(findnsols(5, O, z(M, _, P, O, G), Os)),
  maplist(singleton_list, Os, Rows),
  z_print_table([head(["Object"])|Rows]).



%! rc_predicates is det.
%! rc_predicates(?M, ?G) is det.

rc_predicates :-
  z_graph(M, G),
  rc_predicates(M, G).


rc_predicates(M, G) :-
  aggregate_all(set(P), z(M, _, P, _, G), Ps),
  maplist({M,G}/[P,N]>>z_number_of_triples(M, _, P, _, G, N), Ps, Ns),
  rdf_counts_resources_table0(["Predicate","#occurrences"], Ns, Ps).





% HELPERS %

%! rdf_counts_resources_table0(
%!   +HeaderRow:list(string),
%!   +Counts:list(number),
%!   +Resources:list
%! ) is det.

rdf_counts_resources_table0(HeaderRow, Ns, L) :-
  pairs_keys_values(Pairs, Ns, L),
  z_pairs_table0(HeaderRow, Pairs).


z_pairs_table0(HeaderRow, Pairs) :-
  asc_pairs(Pairs, SortedPairs),
  maplist(pair_inv_list, SortedPairs, DataRows),
  z_print_table([head(HeaderRow)|DataRows]).
