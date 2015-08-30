:- module(
  lod_stats,
  [
    owl_description_size/2, % +Resource:uid
                            % -Size:nonneg
    rdf_description_size/2, % +Resource:rdf_term
                            % -Size:nonneg
    owl_number_of_triples/4, % ?Subject:uid
                             % ?Predicate:uid
                             % ?Object:uid
                             % -Size:nonneg
    rdf_number_of_triples/4 % ?Subject:rdf_term
                        % ?Predicate:iri
                        % ?Object:rdf_term
                        % -Size:nonneg
  ]
).

/** <module> LOD statistics

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(aggregate)).
:- use_module(library(owl/id_store)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_description_size(o,-)).
:- rdf_meta(rdf_number_of_triples(o,r,o,-)).





%! owl_description_size(+Resource:rdf_term, -Size:nonneg) is det.

owl_description_size(S, N):- number_of_triples_id(S, _, _, N).



%! owl_number_of_triples(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Size:nonneg
%! ) is det.

owl_number_of_triples(S, P, O, N):- aggregate_all(count, rdf(S, P, O), N).



%! rdf_description_size(+Resource:rdf_term, -Size:nonneg) is det.

rdf_description_size(S, N):- number_of_triples(S, _, _, N).



%! rdf_number_of_triples(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_triples(S, P, O, N):- aggregate_all(count, rdf3(S, P, O), N).
