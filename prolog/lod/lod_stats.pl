:- module(
  lod_stats,
  [
    description_size/2, % +Resource:rdf_term
                        % -Size:nonneg
    number_of_triples/5 % ?Subject:rdf_term
                        % ?Predicate:iri
                        % ?Object:rdf_term
                        % ?Graph:atom
                        % -Size:nonneg
  ]
).

/** <module> LOD statistics

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(aggregate)).
:- use_module(library(owl/owl_read)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(description_size(o,-)).
:- rdf_meta(number_of_triples(o,r,o,?,-)).




%! description_size(+Resource:rdf_term, -Size:nonneg) is det.

description_size(S, N):-
  number_of_triples(S, _, _, _, N).



%! number_of_triples(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -Size:nonneg
%! ) is det.

number_of_triples(S, P, O, G, N):-
  aggregate_all(count, owl_id(S, P, O, G, _), N).
