:- module(
  geold,
  [
    geold_interpret/0,
    geold_print_feature/1, % ?Feature
    geold_tuple/2,         % +Source, -Tuple
    geold_tuples/2,        % +Source, -Tuples
    geold_unpack/2         % ?FeatureCollection, ?Feature
  ]
).

/** <module> GeoJSON-LD

http://www.opengis.net/ont/geosparql#asWKT
"POINT(-84.22924230000001 39.596629500000006)"

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_array)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(jsonld/wkt)).
:- use_module(library(rdf/rdf_cli)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(geold, 'http://geojsonld.com/vocab#').
:- rdf_register_prefix(gis, 'http://www.opengis.net/ont/geosparql#').
:- rdf_register_prefix(tcco, 'http://triply.cc/ontology/').
:- rdf_register_prefix(wkt, 'http://geojsonld.com/wkt#').

:- rdf_meta
   geold_geometry_class(r),
   geold_geometry_class(r, ?).





geold_context(_{
  coordinates: _{'@id': 'geold:coordinates', '@type': '@array'},
  crs: 'geold:crs',
  geo : 'http://www.opengis.net/ont/geosparql#',
  geold: 'http://geojsonld.com/vocab#',
  geometry: 'geold:geometry',
  'GeometryCollection': 'geold:GeometryCollection',
  'Feature': 'geold:Feature',
  'FeatureCollection': 'geold:FeatureCollection',
  features: 'geold:features',
  'LineString': 'geold:LineString',
  'MultiLineString': 'geold:MultiLineString',
  'MultiPoint': 'geold:MultiPoint',
  'MultiPolygon': 'geold:MultiPolygon',
  'Point': 'geold:Point',
  'Polygon': 'geold:Polygon',
  properties: 'geold:properties',
  type: '@type',
  '@vocab': 'http://example.org/'
}).



%! geold_geometry_class(?C, ?D) is semidet.

geold_geometry_class(C) :-
  geold_geometry_class(C, _).


geold_geometry_class(geold:'MultiPolygon', multipolygon).
geold_geometry_class(geold:'Polygon', polygon).



%! geold_interpret is nondet.

geold_interpret :-
  geold_geometry_class(C, WktName),
  rdfs_instance(I, C),
  rdf_has(I, geold:coordinates, Lex1^^tcco:array),
  string_phrase(array(L), Lex1),
  string_phrase(wkt(WktName, L), Lex2),
  rdf_change(
    I, geold:coordinates, Lex1^^tcco:array,
    object(Lex2^^gis:wktLiteral)
  ),
  fail.
geold_interpret.



%! geold_print_feature(?Feature) is nondet.

geold_print_feature(I) :-
  rdfs_instance(I, geold:'Feature'),
  rdf_print_tree(I).



%! geold_tuple(+Source, -Tuple) is det.

geold_tuple(Source, Tuple) :-
  geold_context(Context),
  json_read_any(Source, D),
  jsonld_tuple_with_context(Context, D, Tuple).



%! geold_tuples(+Source, -Tuples) is det.

geold_tuples(Source, Tuples) :-
  geold_context(Context),
  json_read_any(Source, D),
  aggregate_all(
    set(Tuple),
    jsonld_tuple_with_context(Context, D, Tuple),
    Tuples
  ).



geold_unpack(FeatureCollection, Feature) :-
  rdfs_instance(FeatureCollection, geold:'FeatureCollection'),
  rdf(FeatureCollection, features, Features),
  member(Feature, Features).
