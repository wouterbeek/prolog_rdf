:- module(
  lod_stats,
  [
    rdf_description_size/2, % +Resource:rdf_term
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
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_description_size(o,-)).
:- rdf_meta(rdf_number_of_triples(o,r,o,-)).





%! rdf_description_size(+Resource:rdf_term, -Size:nonneg) is det.

rdf_description_size(S, N):-
  rdf_number_of_triples(S, _, _, N).



%! rdf_number_of_triples(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Size:nonneg
%! ) is det.

rdf_number_of_triples(S, P, O, N):-
  aggregate_all(count, rdf(S, P, O), N).
