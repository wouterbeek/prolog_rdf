:- use_module(library(semweb/rdf_geo)).

:- rdf_meta
   rdf_assert_geojson(r, r, +).

%! rdf_assert_geojson(+S, +Dict, +G) is det.
%
% Assert a GeoJSON dictionary as RDF triples.  Use Well-Known Text
% (WKT) to encode the geometry.

rdf_assert_geojson(S, Dict, G) :-
  atom_string(Type, Dict.type),
  Shape =.. [Type,Dict.coordinates],
  (rdf_wkt(S, Shape, G) -> true ; rdf_assert_wkt(S, Shape, G)).
