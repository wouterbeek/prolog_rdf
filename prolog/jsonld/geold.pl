:- module(
  geold,
  [
    geold_flatten/0,
    geold_flatten/1,       % ?G
    geold_geojson/2,       % +Node, -GeoJson
    geold_print_feature/1, % ?Feature
    geold_rm_feature_collections/0,
    geold_tuple/3,         % +Source, +Alias, -Tuple
    geold_tuple/5,         % +Source, +Alias, +ExtraContext, +ExtraData, -Tuple
    geold_tuples/3,        % +Source, +Alias, -Tuples
    geold_tuples/5         % +Source, +Alias, +ExtraContext, +ExtraData, -Tuples
  ]
).

/** <module> GeoJSON-LD

Because JSON-LD cannot deal with GeoJSON coordinate values, we have to
extend it ourselves.  We do this by adding array support.  This way
JSON-LD can be translated into triples while preserving the array
information.  Later RDF transformations can then be used to interpret
the array as e.g. Well-Known Text (WKT).

@author Wouter Beek
@version 2016/05-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(cli/rc)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(geo/rdf_wkt), []).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_array)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).
:- use_module(library(z/z_term)).

:- rdf_register_prefix(geold, 'http://geojsonld.com/vocab#').

:- rdf_meta
   geold_flatten(r),
   geold_geojson(r, -),
   geold_print_feature(r).





%! geold_context(+Alias, -Context) is det.
%
% The default GeoJSON-LD context.

geold_context(
  Alias,
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
  z_alias_prefix(Alias, Prefix).



%! geold_flatten is det.
%! geold_flatten(?G) is det.

geold_flatten :-
  geold_flatten(_).


geold_flatten(G) :-
  rdf_call_update((
    rdf(S, geold:geometry, B, G),
    % Without the blank node check an already converted literal may
    % appear in the subject position of rdf_has/5, resulting in an
    % exception.
    rdf_is_bnode(B),
    rdf(B, rdf:type, C, G),
    rdf(B, geold:coordinates, Array^^tcco:array, G)
  ), (
    rdf_global_id(_:Name0, C),
    lowercase_atom(Name0, Name),
    rdf_global_id(wkt:Name, D),
    rdf_assert(S, geold:geometry, Array^^D, G),
    rdf_retractall(S, geold:geometry, B, G),
    rdf_retractall(B, rdf:type, C, G),
    rdf_retractall(B, geold:coordinates, Array^^tcco:array, G)
  )).



%! geold_geojson(+Node, -GeoJson) is det.
%
% Emits a description of Node in GeoJSON.

geold_geojson(Node, _{}) :-
  rdfs_instance(Node, geold:'Feature').



%! geold_print_feature(?Feature) is nondet.

geold_print_feature(I) :-
  rdfs_instance(I, geold:'Feature'),
  rc_cbd(I).



%! geold_tuple(+Source, +Alias, -Tuple) is det.
%! geold_tuple(+Source, +Alias, +ExtraContext, +ExtraData, -Tuple) is det.

geold_tuple(Source, Alias, Tuple) :-
  geold_tuple(Source, Alias, _{}, _{}, Tuple).


geold_tuple(Source, Alias, ExtraContext, ExtraData, Tuple) :-
  geold_prepare(Source, Alias, ExtraContext, Context, ExtraData, Data),
  jsonld_tuple_with_context(Context, Data, Tuple).



%! geold_tuples(+Source, +Alias, -Tuples) is det.
%! geold_tuples(+Source, +Alias, +ExtraContext, +ExtraData, -Tuples) is det.

geold_tuples(Source, Alias, Tuples) :-
  geold_tuples(Source, Alias, _{}, _{}, Tuples).


geold_tuples(Source, Alias, ExtraContext, ExtraData, Tuples) :-
  geold_prepare(Source, Alias, ExtraContext, Context, ExtraData, Data),
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

%! geold_prepare(+Source, +Alias, +ExtraContext, -Context, +ExtraData, -Data) is det.

geold_prepare(Source, Alias, ExtraContext, Context, ExtraData, Data) :-
  geold_prepare_context(Alias, ExtraContext, Context),
  geold_prepare_data(Source, ExtraData, Data).



%! geold_prepare_context(+Alias, +ExtraContext, -Context) is det.

geold_prepare_context(Alias, ExtraContext, Context) :-
  geold_context(Alias, Context0),
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
