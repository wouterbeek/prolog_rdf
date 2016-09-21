:- module(
  geold,
  [
    geold_geojson/2, % +Node, -GeoJson
    geold_tuple/2,   % +Source, -Tuple
    geold_tuple/4,   % +Source, +ExtraContext, +ExtraData, -Tuple
    geold_tuples/2,  % +Source, -Tuples
    geold_tuples/4,  % +Source, +ExtraContext, +ExtraData, -Tuples
    reply_geojson/1  % +Dict
  ]
).

/** <module> GeoJSON-LD

Because JSON-LD cannot deal with GeoJSON coordinate values, we have to
extend it ourselves.  We do this by adding array support.  This way
JSON-LD can be translated into triples while preserving the array
information.  Later RDF transformations can then be used to interpret
the array as e.g. Well-Known Text (WKT).

@author Wouter Beek
@version 2016/05-2016/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_json)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_array)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(q/q_wkt)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- qb_alias(geold, 'http://geojsonld.com/vocab#').

:- rdf_meta
   geold_geojson(r, -),
   geold_print_feature(r).





%! geold_context(-Context) is det.
%
% The default GeoJSON-LD context.

geold_context(
  _{
    coordinates: _{'@id': 'geold:coordinates', '@type': 'tcco:array'},
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
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    tcco: 'http://triply.cc/ontology/',
    type: _{'@id': 'rdf:type', '@type': '@id'},
    '@vocab': Prefix
  }
) :-
  q_alias_prefix(nsdef, Prefix).



%! geold_geojson(+Node, -GeoJson) is det.
%
% Emits a description of Node in GeoJSON.

geold_geojson(Node, _{}) :-
  rdfs_instance(Node, geold:'Feature').



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
  geold_tuples(Source, _{}, _{}, Tuples).


geold_tuples(Source, ExtraContext, ExtraData, Tuples) :-
  geold_prepare(Source, ExtraContext, Context, ExtraData, Data),
  aggregate_all(
    set(Tuple),
    jsonld_tuple_with_context(Context, Data, Tuple),
    Tuples
  ).





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

geold_prepare_data(Source, ExtraData, Data2) :-
  json_read_any(Source, Data1),
  (   dict_has_key(features, Data1)
  ->  maplist(
        {ExtraData}/[Data1,Data2]>>dict_put(Data1, ExtraData, Data2),
        Data1.features,
        Data2
      )
  ;   Data2 = Data1.put(ExtraData)
  ).



%! reply_geojson(+Dict) is det.

reply_geojson(Dict) :-
  reply_json_dict(Dict, [content_type('application/vnd.geo+json')]).
