:- module(
  q_graph,
  [
    q_data_graph/2,        % +Refs, -G
    q_graph/2,             % +Refs, -G
    q_graph_table_comps/2, % -HeaderRow, -DataRows
    q_vocab_graph/2,       % +Refs, -G
    q_void_graph/2         % +Refs, -G
  ]
).

/** <module> Quine graph

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(hash_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_stat)).
:- use_module(library(semweb/rdf11)).





%! q_data_graph(+Refs, -G) is det.

q_data_graph(Refs, G) :-
  q_graph([data|Refs], G).



%! q_graph(+Refs, -G) is det.

q_graph(Refs, G) :-
  q_abox_iri([graph|Refs], G).



%! q_graph_table_comps(-HeaderRow, -DataRows) is det.

q_graph_table_comps(HeaderRow, DataRows) :-
  HeaderRow = [bold("Graph"),bold("№ triples"),bold("Store")],
  aggregate_all(set(G), q_view_graph(_, G), Gs),
  maplist(graph_data_row_pair0, Gs, Pairs),
  asc_pairs_values(Pairs, DataRows).


graph_data_row_pair0(
  G,
  NumTriples-[q_graph_term(G),thousands(NumTriples),set(Ms)]
) :-
  once((q_view_graph(M, G), q_number_of_triples(M, G, NumTriples))),
  aggregate_all(set(M0), q_view_graph(M0, G), Ms).



%! q_vocab_graph(+Refs, -G) is det.

q_vocab_graph(Refs, G) :-
  q_graph([vocab|Refs], G).



%! q_void_graph(+Refs, -G) is det.

q_void_graph(Refs, G) :-
  q_graph([void|Refs], G).
