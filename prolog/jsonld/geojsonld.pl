:- module(
  geojsonld,
  [
    geojsonld_tuple/2, % +Source, -Tuple
    geojsonld_tuples/2 % +Source, -Tuples
  ]
).

/** <module> GeoJSON-LD

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_read)).





geojsonld_context(_{
  coordinates: 'geold:coordinates',
  crs: 'geold:crs',
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
  type: '@type'
}).



%! geojsonld_tuple(+Source, -Tuple) is det.

geojsonld_tuple(Source, Tuple) :-
  geojsonld_context(Context),
  json_read_any(Source, D),
  jsonld_tuple_with_context(Context, D, Tuple).



%! geojsonld_tuples(+Source, -Tuples) is det.

geojsonld_tuples(Source, Tuples) :-
  geojsonld_context(Context),
  json_read_any(Source, D),
  aggregate_all(
    set(Tuple),
    jsonld_tuple_with_context(Context, D, Tuple),
    Tuples
  ).
