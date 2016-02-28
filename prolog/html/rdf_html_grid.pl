:- module(
  rdf_html_grid,
  [
    rdf_html_grid//1 % +S
  ]
).

/** <module> RDF HTML grid

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(html/html_grid)).
:- use_module(library(html/rdf_html_stmt)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(rdf11/rdf11)).





rdf_html_grid(G) -->
  {
    gtrace,
    setup_call_cleanup(
      (rdf_tmp_graph(TmpG),rdf_cp_graph(G, TmpG)),
      graph_to_widgets(TmpG, L),
      rdf_unload_graph(TmpG)
    )
  },
  html_grid(L).

graph_to_widgets(G, [H|T]) :-
  pop_triple(G, S, P, O), !,
  H = rdf_html_triple(S, P, O),
  graph_to_widgets(G, T).
graph_to_widgets(_, []).

pop_triple(S, P, O, G) :-
  once(rdf(S, P, O, G)),
  rdf_retractall(S, P, O, G).
