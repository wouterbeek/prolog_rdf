:- module(
  rdf_html_grid,
  [
    rdf_html_grid//1 % +G
  ]
).

/** <module> RDF HTML grid

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(debug)).
:- use_module(library(html/html_grid)).
:- use_module(library(html/rdf_html_stmt)).
:- use_module(library(http/html_write)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(rdf11/rdf11)).

:- rdf_meta
   pop_triple(r, r, o, r),
   pop_triples(r, r, o, r, -),
   rdf_html_grid(r, ?, ?).



rdf_html_grid(G) -->
  {
    setup_call_cleanup(
      (rdf_tmp_graph(TmpG),rdf_cp_graph(G, TmpG)),
      graph_to_widgets(TmpG, L),
      rdf_unload_graph(TmpG)
    )
  },
  html_grid(L).

graph_to_widgets(G, [H|T]) :-
  graph_to_widget(G, H), !,
  graph_to_widgets(G, T).
graph_to_widgets(_, []).

graph_to_widget(G, header(S, P, V2)) :-
  rdf(O, rdf:type, llo:'ValidHttpHeader', G),
  rdf(O, llo:value, V1, G), !,
  header_value(V1, V2, G),
  pop_triple(S, P, O, G),
  rdf_retractall(O, rdf:type, llo:'ValidHttpHeader', G),
  rdf_retractall(O, llo:value, V1, G),
  rdf_retractall(O, llo:raw, _, G).
graph_to_widget(G, triple(S, P, O)) :-
  pop_triple(S, P, O, G).

header_value(V, V, _) :-
  rdf_is_literal(V), !.
header_value(S, O, G) :-
  pop_triple(S, rdf:type, llo:'CacheDirective', G), !,
  pop_triple(S, llo:key, O, G).
header_value(S, access_control_allow_headers(L), G) :-
  rdf(_, llo:'access-control-allow-headers', S, G), !,
  gtrace,
  pop_triples(S, llo:value, _, G, o(L)).
header_value(S, access_control_allow_methods(L), G) :-
  rdf(_, llo:'access-control-allow-methods', S, G), !,
  pop_triples(S, llo:value, _, G, o(L)).
header_value(S, access_control_allow_origin(H), G) :-
  rdf(_, llo:'access-control-allow-origin', S, G), !,
  pop_triple(S, llo:value, H, G).
header_value(S, media_type(Type,Subtype,[Param]), G) :-
  pop_triple(S, rdf:type, llo:'MediaType', G), !,
  pop_triple(S, llo:parameters, O, G),
  parameter(O, Param, G),
  pop_triple(S, llo:subtype, Subtype, G),
  pop_triple(S, llo:type, Type, G).
header_value(S, product(Name,Version), G) :-gtrace,
  pop_triple(S, rdf:type, llo:'Product', G), !,
  pop_triple(S, llo:name, Name, G),
  pop_triple(S, llo:version, Version, G).

parameter(S, param(Key,Value), G) :-
  pop_triple(S, rdf:type, llo:'Parameter', G),
  pop_triple(S, llo:key, Key, G),
  pop_triple(S, llo:value, Value, G).

pop_triple(S, P, O, G) :-
  once(rdf(S, P, O, G)),
  rdf_retractall(S, P, O, G),
  rdf_statistics(triples_by_graph(G,N)),
  debug(rdf(grid), "~D triples left.", [N]).

pop_triples(S, P, O, G, Return) :-
  findall(rdf(S,P,O), rdf(S, P, O, G), Trips),
  Return =.. [Mode,L],
  maplist(triple_return(Mode), Trips, L),
  maplist(rdf_retractall, Trips).

triple_return(s, rdf(S,_,_), S).
triple_return(p, rdf(_,P,_), P).
triple_return(o, rdf(_,_,O), O).
