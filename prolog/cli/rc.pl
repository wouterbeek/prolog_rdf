:- module(
  rc,
  [
    cand_datatype/1, % ?P
    cand_datatype/2, % ?P, ?G
    cand_flatten/0,
    cand_flatten/1,  % ?P
    rc_cbd/1,        % ?S
    rc_cbd/2,        % ?S, ?G
    rc_classes/0,
    rc_graph/1,      % ?G
    rc_graphs/0,
    rc_p/1,          % ?P
    rc_p/2,          % ?P, ?G
    rc_p_no/0,
    rc_p_no/1,       %     ?G
    rc_p_no/2,       % ?P, ?G
    rc_predicates/0,
    rc_predicates/1, % ?G
    rc_root/1,       % ?Node
    rc_root/2,       % ?Node, ?G
    rc_scbd/1,       % ?Node
    rc_scbd/2,       % ?Node, ?G
    rc_tree/1,       % ?Node
    rc_tree/2        % ?Node, ?G
  ]
).

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
:- use_module(library(rdf/rdf_cbd)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(stat/r_ext)).
:- use_module(library(stat/rdf_stat)).
:- use_module(library(stat/rdfs_stat)).
:- use_module(library(yall)).
:- use_module(library(z/z_cbd)).
:- use_module(library(z/z_shape)).

:- rdf_meta
   cand_datatype(r),
   cand_datatype(r, r),
   cand_flatten(r),
   rc_cbd(r),
   rc_cbd(r, r),
   rc_graph(r),
   rc_p(r),
   rc_p(r, r),
   rc_p_no(r),
   rc_p_no(r, r),
   rc_predicates(r),
   rc_root(r),
   rc_root(r, r),
   rc_scbd(o),
   rc_scbd(o, r),
   rc_tree(r),
   rc_tree(r, r).





%! cand_datatype(?P) is nondet.
%! cand_datatype(?P, ?G) is nondet.
%
% Shows the candidate datatypes per predicate term.

cand_datatype(P) :-
  cand_datatype(P, _).


cand_datatype(P, G) :-
  rdf_predicate(P, G),
  rdf_datatypes_compat(P, G, Ds),
  maplist(list_split, Rows, Ds),
  rdf_print_table([head([P])|Rows]).



%! cand_flatten is nondet.
%! cand_flatten(?P) is nondet.
%
% Candidates for [[rdf_flatten/1]].

cand_flatten :-
  findall([P,N], (cand_flatten(P), rdf_number_of_triples(_, P, _, N)), Rows),
  rdf_print_table([head(["Predicate","#occurrences"])|Rows]).


cand_flatten(P) :-
  rdf_predicate(P),
  forall(rdf(_, P, O), rdf_is_bnode(O)),
  once(rdf(_, P, _)),
  rc_next_p(P).

rc_next_p(P) :-
  aggregate_all(set(Q), (rdf(_, P, X), rdf(X, Q, _)), Qs),
  maplist(singleton_list, Qs, Rows),
  format(string(Lbl), "Next predicates of ~a", [P]),
  rdf_print_table([head([Lbl])|Rows]).



%! rc_cbd(?S) is det.
%! rc_cbd(?S, ?G) is det.
%
% Print the Concise-Bounded Description (CBD) of subject terms.

rc_cbd(S) :-
  rc_cbd(S, _).


rc_cbd(S, G) :-
  rdf_cbd(S, G, Triples),
  rdf_print_triples(Triples).



%! rc_classes is det.

rc_classes :-
  findall(N-C, (rdfs_class(C), rdfs_number_of_instances(C, N)), Pairs),
  rdf_pairs_table0(["Class","#Instances"], Pairs).



%! rc_graph(?G) is nondet.

rc_graph(G) :-
  rdf_graph(G),
  rdf_print_graph(G).



%! rc_graphs is det.

rc_graphs :-
  findall(N-G, rdf_statistics(triples_by_graph(G,N)), Pairs),
  rdf_pairs_table0(["Graph","#Triples"], Pairs).



%! rc_p(?P) is nondet.
%! rc_p(?P, ?G) is nondet.

rc_p(P) :-
  rc_p(P, _).


rc_p(P, G) :-
  rc_p_no(P, G),
  cand_datatype(P, G).



%! rc_p_no is nondet.
%! rc_p_no(?G) is nondet.
%
% Prints an overview of how many distinct objects there are for a
% given predicate term.

rc_p_no :-
  rc_p_no(_).


rc_p_no(G) :-
  rdf_graph(G),
  aggregate_all(set(P), rdf_predicate(P, G), Ps),
  maplist({G}/[P,N]>>rdf_number_of_objects(_, P, G, N), Ps, Ns),
  rdf_counts_resources_table0(["Predicate","#Objects"], Ns, Ps).


%! rc_p_no(?P, ?G) is nondet.
%
% Prints an overview of how often each object term occurs.

rc_p_no(P, G) :-
  rdf_predicate(P, G),
  (   \+ ((rdf(S1, P, O, G), rdf(S2, P, O, G), S1 \== S2))
  ->  rc_p_no_abbr(P, G, "No reuse of object terms.")
  ;   aggregate_all(set(O), rdf(_, P, O, G), Os),
      (   length(Os, Len),
          Len > 1000
      ->  rc_p_no_abbr(P, G, "Too many unique object terms.")
      ;   maplist({P,G}/[O,N]>>rdf_number_of_subjects(P, O, G, N), Os, Ns),
          rdf_counts_resources_table0(["Object","#Occurrences"], Ns, Os)
      )
  ).

rc_p_no_abbr(P, G, Msg) :-
  ansi_format(user_output, [fg(yellow)], "~s~n", [Msg]),
  once(findnsols(5, O, rdf(_, P, O, G), Os)),
  maplist(singleton_list, Os, Rows),
  rdf_print_table([head(["Object"])|Rows]).



%! rc_predicates is det.
%! rc_predicates(?G) is det.

rc_predicates :-
  rc_predicates(_).


rc_predicates(G) :-
  aggregate_all(set(P), rdf(_, P, _, G), Ps),
  maplist([P,N]>>rdf_number_of_triples(_, P, _, G, N), Ps, Ns),
  rdf_counts_resources_table0(["Predicate","#Occurrences"], Ns, Ps).



%! rc_root(?Node) is det.
%! rc_root(?Node, ?G) is det.
%
% Print the tree for an RDF root node.

rc_root(Node) :-
  rc_root(Node, _).


rc_root(Node, G) :-
  rdf_root(Node, G),
  z_tree(Node, G, Triples),
  rdf_print_triples(Triples).



%! rc_scbd(?Node) is det.
%! rc_scbd(?Node, ?G) is det.
%
% Print the Symmetric CBD (SCBD) for an RDF node.

rc_scbd(Node) :-
  rc_scbd(Node, _).


rc_scbd(Node, G) :-
  z_scbd(Node, G, Triples),
  rdf_print_triples(Triples).



%! rc_tree(?Node) is det.
%! rc_tree(?Node, ?G) is det.
%
% Print the tree for a subject term.

rc_tree(Node) :-
  rc_tree(Node, _).


rc_tree(Node, G) :-
  z_tree(Node, G, Triples),
  rdf_print_triples(Triples).





% HELPERS %

%! rdf_counts_resources_table0(
%!   +HeaderRow:list(string),
%!   +Counts:list(number),
%!   +Resources:list
%! ) is det.

rdf_counts_resources_table0(HeaderRow, Ns, L) :-
  pairs_keys_values(Pairs, Ns, L),
  rdf_pairs_table0(HeaderRow, Pairs).


rdf_pairs_table0(HeaderRow, Pairs) :-
  asc_pairs(Pairs, SortedPairs),
  maplist(pair_inv_list, SortedPairs, DataRows),
  rdf_print_table([head(HeaderRow)|DataRows]).
