:- module(
  q_graph,
  [
    q_graph_pp/2,         % +EncG, -G
    q_graph_table_comps/2 % -HeaderRow, -DataRows
  ]
).

/** <module> Quine graph

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(base64)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_stat)).
:- use_module(library(semweb/rdf11)).





%! q_graph_pp(+EncG, -G) is det.

q_graph_pp(EncG, G) :-
  rdf_global_id(Alias:EncLocal, EncG),
  base64(Local, EncLocal),
  rdf_global_id(Alias:Local, G).



%! q_graph_table_comps(-HeaderRow, -DataRows) is det.

q_graph_table_comps(HeaderRow, DataRows) :-
  HeaderRow = [bold("Graph"),bold("№ triples"),bold("Store")],
  aggregate_all(set(G), q_view_graph(G), Gs),
  maplist(graph_data_row_pair0, Gs, Pairs),
  asc_pairs_values(Pairs, DataRows).


graph_data_row_pair0(
  G,
  NumTriples-[dcg_q_print_graph_term(G),thousands(NumTriples),set(Ms)]
) :-
  once((q_view_graph(M, G), q_number_of_triples(M, G, NumTriples))),
  aggregate_all(set(M0), q_view_graph(M0, G), Ms).
