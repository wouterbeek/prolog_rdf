:- module(
  rdf_wkt,
  [
    rdf_assert_geojson/3 % +S, +G, +Dict
  ]
).

/** <module> RDF WKT

This module adds Well-Known Text (WKT) support to the SWI-Prolog
Semantic Web library.

@author Wouter Beek
@version 2017/01-2017/04
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(geo/wkt_generate)).
:- use_module(library(geo/wkt_parse)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(uuid)).

:- rdf_meta
   rdf_assert_geojson(r, r, +),
   rdf_assert_wkt(r, +, r).





%! rdf_assert_geojson(+S, +G, +Dict) is det.
%
% Assert a GeoJSON dictionary as RDF triples.  Use Well-Known Text
% (WKT) to encode the geometry.

rdf_assert_geojson(S, G, Dict) :-
  atom_string(Type, Dict.type),
  Shape =.. [Type,Dict.coordinates],
  (rdf_wkt(S, Shape, G) -> true ; rdf_assert_wkt(S, Shape, G)).
