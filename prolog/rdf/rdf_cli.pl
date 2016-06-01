:- module(
  rdf_cli,
  [
    rdf_print_graphs/0,
    rdf_print_root/1,   % ?S
    rdf_print_root/2,   % ?S, ?G
    rdf_print_root/3,   % ?S, ?G, +Opts
    rdf_print_tree/1,   % ?S
    rdf_print_tree/2,   % ?S, ?G
    rdf_print_tree/3    % ?S, ?G, +Opts
  ]
).

/** <module> RDF CLI

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_print_root(r),
   rdf_print_root(r, r),
   rdf_print_root(r, r, +),
   rdf_print_tree(r),
   rdf_print_tree(r, r),
   rdf_print_tree(r, r, +).





%! rdf_print_graphs is det.

rdf_print_graphs :-
  findall(N-G, rdf_statistics(triples_by_graph(G,N)), Pairs),
  asc_pairs(Pairs, SortedPairs),
  maplist(pair_inv_list, SortedPairs, Rows),
  rdf_print_table([head(["Graph","#triples"])|Rows]).



%! rdf_print_root(?S) is det.
%! rdf_print_root(?S, ?G) is det.
%! rdf_print_root(?S, ?G, +Opts) is det.

rdf_print_root(S) :-
  rdf_print_root(S, _).


rdf_print_root(S, G) :-
  rdf_print_root(S, G, []).


rdf_print_root(Root, G, Opts) :-
  rdf_root(Root, G),
  rdf_tree(Root, G, Quads),
  dcg_with_output_to(current_output, dcg_print_quads(Quads, Opts)).



%! rdf_print_tree(?S) is det.
%! rdf_print_tree(?S, ?G) is det.
%! rdf_print_tree(?S, ?G, +Opts) is det.

rdf_print_tree(S) :-
  rdf_print_tree(S, _).


rdf_print_tree(S, G) :-
  rdf_print_tree(S, G, []).


rdf_print_tree(Node, G, Opts) :-
  rdf_tree(Node, G, Quads),
  dcg_with_output_to(current_output, dcg_print_quads(Quads, Opts)).
