:- module(
  geold,
  [
    geojson_feature_collection/2, % +Features, -FeatureCollection
    geold_geojson/2,     % +Node, -GeoJson
    geold_tuple/2,       % +Source, -Tuple
    geold_tuple/4,       % +Source, +ExtraContext, +ExtraData, -Tuple
    geold_tuples/2,      % +Source, -Tuples
    geold_tuples/4,      % +Source, +ExtraContext, +ExtraData, -Tuples
    subject_to_geojson/5 % +M, +Properties, +G, +S, -Feature
  ]
).

/** <module> GeoLD: GeoJSON + JSON-LD

Because JSON-LD cannot deal with GeoJSON coordinate values, we have to
extend it ourselves.  We do this by adding array support.  This way
JSON-LD can be translated into triples while preserving the array
information.  Later RDF transformations can then be used to interpret
the array as e.g. Well-Known Text (WKT).

@author Wouter Beek
@version 2016/05-2016/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_json)).
:- use_module(library(html/qh_ui)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_array)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(q/q_wkt)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs), [
     rdfs_individual_of/2
   ]).
:- use_module(library(yall)).

:- rdf_create_alias(geold, 'http://geojsonld.com/vocab#').

:- rdf_meta
   geold_geojson(r, -),
   geold_print_feature(r).





%! geojson_feature_collection(+Features, -FeatureCollection) is det.

geojson_feature_collection(Features, FeatureCollection) :-
  FeatureCollection = _{features: Features, type: "FeatureCollection"}.



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
  rdf_alias_prefix(nsdef, Prefix).



%! geold_geojson(+Node, -GeoJson) is det.
%
% Emits a description of Node in GeoJSON.

geold_geojson(Node, _{}) :-
  rdfs_individual_of(Node, geold:'Feature').



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
  (   dict_key(Data1, features)
  ->  maplist(
        {ExtraData}/[Data1,Data2]>>dict_put(Data1, ExtraData, Data2),
        Data1.features,
        Data2
      )
  ;   Data2 = Data1.put(ExtraData)
  ).



%! subject_to_geojson(+M, +Properties, +G, +S, -Feature) is det.

subject_to_geojson(M, Properties, G, S, Feature) :-
  once(gis:subject_to_geometry(M, S, Array, Name, G)),
  capitalize_atom(Name, CName),
  subject_to_geojson_properties(M, Properties, G, S, Dict),
  Feature = _{
    geometry: _{coordinates: Array, type: CName},
    properties: Dict,
    type: "Feature"
  }.


subject_to_geojson_properties(_, false, _, S, _{'@id': S}) :- !.
subject_to_geojson_properties(M, true, G, S, Dict) :-
  aggregate_all(
    set(Key-Val),
    (
      q(M, S, P, O, G),
      \+ q_is_bnode(O),
      \+ rdf_equal(geold:geometry, P),
      q_iri_local(P, Key),
      (   q_is_literal(O)
      ->  rdf_literal_lexical_form(O, Val0),
          json_escape(Val0, Val)
      ;   q_iri_local(O, Val)
      )
    ),
    Pairs
  ),
  group_pairs_by_key(Pairs, GroupedPairs0),
  maplist(pair_flatten_singleton, GroupedPairs0, GroupedPairs),
  (   html_to_atom(\qh_p_os_table(GroupedPairs), Val)
  ->  Pair = popupContent-Val
  ;   Pair = '@id'-S
  ),
  dict_pairs(Dict, [Pair|GroupedPairs]).
