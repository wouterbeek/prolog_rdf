:- module(
  geold,
  [
    geold_array2wkt/0,
    geold_print_feature/1, % ?Feature
    geold_rm_feature_collections/0,
    geold_tuple/2,  % +Source, -Tuple
    geold_tuple/4,  % +Source, +ExtraContext, +ExtraData, -Tuple
    geold_tuples/2, % +Source, -Tuples
    geold_tuples/4  % +Source, +ExtraContext, +ExtraData, -Tuples
  ]
).

/** <module> GeoJSON-LD

http://www.opengis.net/ont/geosparql#asWKT
"POINT(-84.22924230000001 39.596629500000006)"

@author Wouter Beek
@version 2016/05-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(cli/rc)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(geo/wkt)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_array)).
:- use_module(library(jsonld/jsonld_read)).
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
   geold_geometry_class(r, ?),
   geold_print_feature(r).





%! geold_array2wkt is nondet.
%
% Transforms JSON-LD arrays (see module [[jsonld_array]]) into
% Well-Known Text literals (see module [[wkt]]).

geold_array2wkt :-
  geold_geometry_class(C, WktName),
  rdfs_instance(I, C),
  rdf_has(I, geold:coordinates, Lex1^^tcco:array),
  string_phrase(array(Array), Lex1),
  string_phrase(wkt(WktName, Array), Lex2),
  rdf_change(
    I, geold:coordinates, Lex1^^tcco:array,
    object(Lex2^^gis:wktLiteral)
  ),
  fail.
geold_array2wkt.



%! geold_context(-Context) is det.
%
% The default GeoJSON-LD context.

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



%! geold_geometry_class(?C) is semidet.
%! geold_geometry_class(?C, ?Name) is semidet.

geold_geometry_class(C) :-
  geold_geometry_class(C, _).


geold_geometry_class(geold:'MultiPolygon', multipolygon).
geold_geometry_class(geold:'Point', point).
geold_geometry_class(geold:'Polygon', polygon).



%! geold_print_feature(?Feature) is nondet.

geold_print_feature(I) :-
  rdfs_instance(I, geold:'Feature'),
  rc_cbd(I).



%! geold_tuple(+Source, -Tuple) is det.
%! geold_tuple(+Source, +ExtraContext, +ExtraData, -Tuple) is det.

geold_tuple(Source, Tuple) :-
  geold_tuple(Source, _{}, _{}, Tuple).


geold_tuple(Source, ExtraContext, ExtraData, Tuple) :-
  geold_prepare(Source, ExtraContext, Context, ExtraData, Data),
  jsonld_tuple_with_context(Context, Data, Tuple).



%! geold_tuples(+Source, -Tuples) is det.
%! geold_tuples(+Source, +ExtraContext, +ExtraData, -Tuples) is det.

geold_tuples(Source, Tuples) :-
  geold_tuples(Source, _{}, Tuples).


geold_tuples(Source, ExtraContext, ExtraData, Tuples) :-
  geold_prepare(Source, ExtraContext, Context, ExtraData, Data),
  aggregate_all(
    set(Tuple),
    jsonld_tuple_with_context(Context, Data, Tuple),
    Tuples
  ).



%! geold_rm_feature_collections is det.
%
% Remove all GeoJSON FeatureCollections, since these are mere
% artifacts.

geold_rm_feature_collections :-
  rdf_rm_col(geold:features),
  rdf_rm(_, rdf:type, geold:'FeatureCollection').





% HELPERS %

%! geold_prepare(+Source, +ExtraContext, -Context, +ExtraData, -Data) is det.

geold_prepare(Source, ExtraContext, Context, ExtraData, Data) :-
  geold_prepare_context(ExtraContext, Context),
  geold_prepare_data(Source, ExtraData, Data).



%! geold_prepare_context(+ExtraContext, -Context) is det.

geold_prepare_context(ExtraContext, Context) :-
  geold_context(Context0),
  Context = Context0.put(ExtraContext).



%! geold_prepare_data(+Source, +ExtraData, -Data) is det.

geold_prepare_data(Source, ExtraData, Data) :-
  json_read_any(Source, Data0),
  Data = Data0.put(ExtraData).
