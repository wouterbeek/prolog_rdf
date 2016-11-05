:- module(
  q_graph,
  [
    q_graph_label/2,      % +G, -Lbl
    q_graph_source/3,     % +M, +G, -Source
    q_graph_table_comps/2 % -HeaderRow, -DataRows
  ]
).

/** <module> Quine graph

@author Wouter Beek
@version 2016/08, 2016/10-2016/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(hash_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_dataset_db)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).





%! q_graph_label(+G, -Lbl) is det.

q_graph_label(G, Str) :-
  q_dataset_graph(D, G),
  q_dataset_default_graph(D, DefG),
  q_pref_label(hdt, G, Lit, DefG),
  q_literal_string(Lit, Str).



%! q_graph_source(+M, +G, -Source) is det.

q_graph_source(hdt, G, Source) :- !,
  q_file_graph(Source, G).
q_graph_source(trp, G, Source) :-
  rdf_graph_property(G, source(Source)).



%! q_graph_table_comps(-HeaderRow, -DataRows) is det.

q_graph_table_comps(HeaderRow, DataRows) :-
  HeaderRow = ["Graph","№ triples","Store"],
  aggregate_all(set(G), q_graph(G), Gs),
  maplist(graph_data_row_pair0, Gs, Pairs),
  desc_pairs_values(Pairs, DataRows).

graph_data_row_pair0(
  G,
  NumTriples-[q_graph_term(G),thousands(NumTriples),set(Ms)]
) :-
  (q_number_of_triples(_, G, NumTriples) -> true ; NumTriples = inf),
  aggregate_all(set(M0), q_view_graph(M0, G), Ms).
