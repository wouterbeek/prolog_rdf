:- module(
  virt_geo,
  [
    virt_geo/1, % +G
    virt_geo/2  % +File1, +File2
  ]
).

/** <module> Virtuoso geometry support

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(q/qb)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qu)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- qb_alias(geosparql, 'http://www.opengis.net/ont/geosparql#').
:- qb_alias(ngeo, 'http://geovocab.org/geometry#').

:- rdf_meta
   virt_geo(r).





%! virt_geo(+G) is det.
%! virt_geo(+File1, +File2) is det.

virt_geo(G) :-
  M = rdf
  rdf_call_update((
    q(M, S, geold:geometry, Lit, P, G)
  ), (
    q_literal_lex(Lit, Lex),
    qb(M, S, geosparql:asWKT, Lex^^geosparql:wktLiteral, G),
    qb_instance(M, S, ngeo:'Geometry', G),
    q_retractall(M, S, P, Lit, G)
  )).


virt_geo(File1, File2) :-
  rdf_call_on_graph(File1, {File2}/[G,M,M]>>virt_geo0(G, File2)).

virt_geo0(G, File) :-
  virt_geo(G),
  rdf_write_to_sink(File, G, [rdf_format(ntriples)]).
