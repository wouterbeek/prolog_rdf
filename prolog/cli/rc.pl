:- module(
  rc,
  [
    rc_cbd/1,  % ?Node
    rc_cbd/2,  % ?Node, +Opts
    rc_gs/0,
    rc_p/2,    % ?P, ?G
    rc_p_ds/2, % ?P, ?G
    rc_p_no/1, %     ?G
    rc_p_no/2, % ?P, ?G
    rc_root/1, % ?Node
    rc_root/2, % ?Node, +Opts
    rc_scbd/1, % ?Node
    rc_scbd/2, % ?Node, +Opts
    rc_tree/1, % ?Node
    rc_tree/2  % ?Node, +Opts
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
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_cbd)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(stat/r_ext)).
:- use_module(library(stat/rdf_stat)).
:- use_module(library(yall)).

:- rdf_meta
   rc_cbd(r),
   rc_cbd(r, +),
   rc_p_info(r, r),
   rc_p_types(r, r),
   rc_p_o(r),
   rc_p_no(r, r),
   rc_root(r),
   rc_root(r, +),
   rc_scbd(r),
   rc_scbd(r, +),
   rc_tree(r),
   rc_tree(r, +).





%! rc_cbd(?Node) is det.
%! rc_cbd(?Node, +Opts) is det.

rc_cbd(Node) :-
  rc_cbd(Node, _{}).


rc_cbd(Node, Opts) :-
  cbd(Node, Triples),
  rdf_print_triples(Triples, Opts).



%! rc_gs is det.

rc_gs :-
  findall(N-G, rdf_statistics(triples_by_graph(G,N)), Pairs),
  asc_pairs(Pairs, SortedPairs),
  maplist(pair_inv_list, SortedPairs, Rows),
  rdf_print_table([head(["Graph","#triples"])|Rows]).



%! rc_p(?P, ?G) is nondet.

rc_p(P, G) :-
  rdf_predicate_values(P, G),
  rdf_predicate_types(P, G).



%! rc_p_ds(?P, ?G) is nondet.

rc_p_ds(P, G) :-
  rdf_predicate(P, G),
  rdf_datatypes_compat(P, G, Ds),
  maplist(list_split, Rows, Ds),
  rdf_print_table([head([P])|Rows]).



%! rc_p_no(?G) is nondet.
%
% Prints an overview of how many distinct objects there are for a
% given predicate term.

rc_p_no(G) :-
  rdf_graph(G),
  findall(
    N-P,
    (rdf_predicate(P, G), rdf_number_of_objects(_, P, G, N)),
    Pairs
  ),
  asc_pairs(Pairs, SortedPairs),
  maplist(pair_inv_list, SortedPairs, Rows),
  rdf_print_table([head(["Predicate","#objects"])|Rows]).


%! rc_p_no(?P, ?G) is nondet.
%
% Prints an overview of how often each object term occurs.

rc_p_no(P, G) :-
  rdf_predicate(P, G),
  aggregate_all(set(O), rdf(_, P, O, G), Os),
  maplist({P,G}/[O,N]>>rdf_number_of_subjects(P, O, G, N), Os, Ns),
  pairs_keys_values(Pairs, Ns, Os),
  asc_pairs(Pairs, OrderedPairs),
  maplist(pair_inv_list, OrderedPairs, Rows),
  rdf_print_table([head(["Object","#occurrences"])|Rows]).



%! rc_root(?Node) is det.
%! rc_root(?Node, +Opts) is det.

rc_root(Node) :-
  rc_root(Node, _{}).


rc_root(Root, Opts) :-
  rdf_root(Root),
  rdf_tree(Root, Triples),
  dcg_with_output_to(current_output, dcg_print_triples(Triples, Opts)).



%! rc_scbd(?Node) is det.
%! rc_scbd(?Node, +Opts) is det.

rc_scbd(Node) :-
  rc_scbd(Node, _{}).


rc_scbd(Node, Opts) :-
  scbd(Node, Triples),
  rdf_print_triples(Triples, Opts).



%! rc_tree(?Node) is det.
%! rc_tree(?Node, +Opts) is det.

rc_tree(Node) :-
  rc_tree(Node, _{}).


rc_tree(Node, Opts) :-
  rdf_tree(Node, Triples),
  dcg_with_output_to(current_output, dcg_print_triples(Triples, Opts)).
