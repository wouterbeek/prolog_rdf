:- module(
  rdf_cli,
  [
    rdf_print_cbd/1,    % ?Node
    rdf_print_cbd/2,    % ?Node, +Opts
    rdf_print_graphs/0,
    rdf_print_root/1,   % ?Node
    rdf_print_root/2,   % ?Node, +Opts
    rdf_print_scbd/1,   % ?Node
    rdf_print_scbd/2,   % ?Node, +Opts
    rdf_print_tree/1,   % ?Node
    rdf_print_tree/2    % ?Node, +Opts
  ]
).

/** <module> RDF CLI

@author Wouter Beek
@version 2016/05-2016/06
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_cbd)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_print_cbd(r),
   rdf_print_cbd(r, +),
   rdf_print_root(r),
   rdf_print_root(r, +),
   rdf_print_scbd(r),
   rdf_print_scbd(r, +),
   rdf_print_tree(r),
   rdf_print_tree(r, +).





%! rdf_print_cbd(?Node) is det.
%! rdf_print_cbd(?Node, +Opts) is det.

rdf_print_cbd(Node) :-
  rdf_print_cbd(Node, _{}).


rdf_print_cbd(Node, Opts) :-
  cbd(Node, Cbd),
  rdf_print_triples(Cbd, Opts).



%! rdf_print_graphs is det.

rdf_print_graphs :-
  findall(N-G, rdf_statistics(triples_by_graph(G,N)), Pairs),
  asc_pairs(Pairs, SortedPairs),
  maplist(pair_inv_list, SortedPairs, Rows),
  rdf_print_table([head(["Graph","#triples"])|Rows]).



%! rdf_print_root(?Node) is det.
%! rdf_print_root(?Node, +Opts) is det.

rdf_print_root(Node) :-
  rdf_print_root(Node, _{}).


rdf_print_root(Root, Opts) :-
  rdf_root(Root),
  rdf_tree(Root, Triples),
  dcg_with_output_to(current_output, dcg_print_quads(Triples, Opts)).



%! rdf_print_scbd(?Node) is det.
%! rdf_print_scbd(?Node, +Opts) is det.

rdf_print_scbd(Node) :-
  rdf_print_scbd(Node, _{}).


rdf_print_scbd(Node, Opts) :-
  scbd(Node, Scbd),
  rdf_print_triples(Scbd, Opts).



%! rdf_print_tree(?Node) is det.
%! rdf_print_tree(?Node, +Opts) is det.

rdf_print_tree(Node) :-
  rdf_print_tree(Node, _{}).


rdf_print_tree(Node, Opts) :-
  rdf_tree(Node, Triples),
  dcg_with_output_to(current_output, dcg_print_triples(Triples, Opts)).
