:- module(
  geojsonld,
  [
    geojsonld_geometry_class/1, % ?C
    geojsonld_geometry_class/2, % ?C, ?D
    geojsonld_interpret/0,
    geojsonld_interpret/1,      % ?I
    geojsonld_tuple/2,          % +Source, -Tuple
    geojsonld_tuples/2          % +Source, -Tuples
  ]
).

/** <module> GeoJSON-LD

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_array)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(jsonld/wkt)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(geold, 'http://geojsonld.com/vocab#').
:- rdf_register_prefix(wkt, 'http://geojsonld.com/wkt#').

:- rdf_meta
   geojsonld_geometry_class(r),
   geojsonld_geometry_class(r, r),
   geojsonld_interpret(r).





geojsonld_context(_{
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
  '@vocab': 'http://example.com/'
}).



%! geojsonld_geometry_class(?C) is semidet.
%! geojsonld_geometry_class(?C, ?D) is nondet.

geojsonld_geometry_class(C) :-
  geojsonld_geometry_class(C, _).


geojsonld_geometry_class(geold:'MultiPolygon', wkt:multipolygon).
geojsonld_geometry_class(geold:'Polygon', wkt:polygon).



%! geojsonld_interpret is nondet.
%! geojsonld_interpret(?I) is nondet.

geojsonld_interpret :-
  forall(geojsonld_interpret(_), true).


geojsonld_interpret(I) :-
  geojsonld_geometry_class(C),
  rdfs_instance(I, C),
  rdf_has(I, geold:coordinates, Lex1^^xsd:string),
  string_phrase(array(L), Lex1),
  geojsonld_geometry_class(C, D),
  string_phrase(wkt(D, L), Lex2),
  rdf_change(I, geold:coordinates, Lex1^^xsd:string, object(Lex2^^D)).



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
