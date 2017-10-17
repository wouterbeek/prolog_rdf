:- module(
  rdf_viz,
  [
    rdf_viz/2 % +Out, +G
  ]
).
:- reexport(library(graph/dot)).

/** <module> RDF visualization

@author Wouter Beek
@version 2017/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_print)).

:- rdf_meta
   rdf_viz(+, r).





%! rdf_viz(+Out:stream, G:iri) is det.

rdf_viz(Out, G) :-
  format_debug(dot, Out, "digraph G {"),
  aggregate_all(set(Node), rdf_node(Node, G), Nodes),
  maplist(export_node(Out), Nodes),
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O, G), Edges),
  maplist(export_edge(Out), Edges),
  format_debug(dot, Out, "}").

export_node(Out, Node) :-
  dot_id(Node, Id),
  dcg_with_output_to(string(Label), rdf_dcg_term(Node)),
  dot_node(Out, Id, [label(Label)]).

export_edge(Out, rdf(S,P,O)) :-
  maplist(dot_id, [S,O], [SId,OId]),
  dcg_with_output_to(string(Label), rdf_dcg_iri(P)),
  dot_edge(Out, SId, OId, [label(Label)]).
