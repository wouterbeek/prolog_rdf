:- module(
  rdfs_stats,
  [
    rdfs_number_of_instances/2 % +Class:rdf_term
                               % -Count:nonneg
  ]
).

/** <module> RDFS statistics

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdfs), [rdfs_individual_of/2]).

:- rdf_meta(rdf_number_of_instances(r,-)).





%! rdfs_number_of_instances(+Class:rdf_term, -Count:nonneg) is det.

rdfs_number_of_instances(C, N):-
  aggregate_all(count, rdfs_individual_of(_, C), N).
