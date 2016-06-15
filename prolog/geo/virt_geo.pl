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

:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- rdf_register_prefix(geosparql, 'http://www.opengis.net/ont/geosparql#').
:- rdf_register_prefix(ngeo, 'http://geovocab.org/geometry#').

:- rdf_meta
   virt_geo(r).





%! virt_geo(+G) is det.
%! virt_geo(+File1, +File2) is det.

virt_geo(G) :-
  rdf_call_update((
    rdf_has(S, geold:geometry, Lit, P, G)
  ), (
    rdf_literal_lex(Lit, Lex),
    rdf_assert(S, geosparql:asWKT, Lex^^geosparql:wktLiteral, G),
    rdf_assert_instance(S, ngeo:'Geometry', G),
    rdf_retractall(S, P, Lit, G)
  )).


virt_geo(File1, File2) :-
  rdf_call_on_graph(File1, {File2}/[G,M,M]>>virt_geo0(G, File2)).

virt_geo0(G, File) :-
  virt_geo(G),
  rdf_write_to_sink(File, G, [rdf_format(ntriples)]).
