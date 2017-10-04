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

:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(trp/trp_update)).
:- use_module(library(yall)).

:- rdf_create_alias(geosparql, 'http://www.opengis.net/ont/geosparql#').
:- rdf_create_alias(ngeo, 'http://geovocab.org/geometry#').

:- rdf_meta
   virt_geo(r).





%! virt_geo(+G) is det.
%! virt_geo(+File1, +File2) is det.

virt_geo(G) :-
  M = rdf
  rdf_call_update((
    q(M, S, geold:geometry, Lit, P, G)
  ), (
    rdf_literal(Lit, _, _, Lex),
    qb(M, S, geosparql:asWKT, Lex^^geosparql:wktLiteral, G),
    qb_instance(M, S, ngeo:'Geometry', G),
    q_retractall(M, S, P, Lit, G)
  )).


virt_geo(File1, File2) :-
  rdf_call_on_graph(File1, {File2}/[G,M,M]>>virt_geo0(G, File2)).

virt_geo0(G, File) :-
  virt_geo(G),
  rdf_write_to_sink(File, G, [rdf_media_type(application/'n-triples')]).
