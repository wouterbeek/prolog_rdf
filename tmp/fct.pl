:- module(
  fct,
  [
    fct_label/3 % +Str, -Score, -Iri
  ]
).

/** <module> FCT

@author Wouter Beek
@see http://dbpedia.org/fct/
@version 2016/07, 2016/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(os/io)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_deref)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(settings)).
:- use_module(library(uri_ext)).

:- setting(
     endpoint_host,
     atom,
     'dbpedia.org',
     "The host name of the FCT endpoint."
   ).
:- setting(
     endpoint_path,
     list(atom),
     [services,rdf,'iriautocomplete.get'],
     "The path of the FCT endpoint."
   ).
:- setting(
     endpoint_scheme,
     oneof([http,https]),
     http,
     "The scheme of the FCT endpoint."
   ).





%! fct_label(+Str, -Iri) is nondet.

fct_label(Str, Score, Iri) :-
  setting(fct:endpoint_scheme, Scheme),
  setting(fct:endpoint_host, Host),
  setting(fct:endpoint_path, PathComps),
  atomic_list_concat([''|PathComps], /, Path),
  uri_query_components(Query, [lbl(Str)]),
  uri_components(Url, uri_components(Scheme,Host,Path,Query,_)),
  % json_read_any/[2,3] cannot be used here because the `Accept`
  % header must be `*` in order to retrieve JSON.
  setup_call_cleanup(
    http_open(Url, In, [request_header('Accept'='*')]),
    json_read_dict(In, Results),
    close(In)
  ),
  aggregate_all(set(Pair), result_pair(Results, Pair), Pairs),
  desc_pairs(Pairs, SortedPairs),
  member(Score-Iri, SortedPairs).

result_pair(Results, Score-Iri) :-
  member(Result, Results.results),
  atom_string(Iri, Result),
  uri_is_global(Iri),
  rdf_deref_triples(Iri, Triples),
  aggregate_all(count, is_location(Iri, Triples), NumCs),
  aggregate_all(count, q_deref_triple(Iri, _), NumTriples),
  Score is NumTriples + 100 * NumCs,
  Score > 0.

is_location(Iri, Triples) :-
  rdf_prefix_member(
    C,
    [
      dbo:'Location',
      dbo:'Place',
      dbo:'PopulatedPlace',
      dbo:'Settlement',
      dbo:'Village',
      dby:'YagoGeoEntity',
      schema:'Place',
      wgs84:'SpatialThing'
    ]
  ),
  rdf_prefix_memberchk(rdf(Iri,rdf:type,C), Triples).
