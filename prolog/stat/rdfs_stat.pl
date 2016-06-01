:- module(
  rdfs_stat,
  [
    rdfs_number_of_classes/1,   %     -N
    rdfs_number_of_instances/2, % +C, -N
    rdfs_number_of_properties/1 %     -N
  ]
).

/** <module> RDFS statistics

@author Wouter Beek
@version 2015/12-2016/01, 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_number_of_instances(r, -).





%! rdfs_number_of_classes(-Count:nonneg) is det.

rdfs_number_of_classes(N) :-
  aggregate_all(count, rdfs_class(_), N).



%! rdfs_number_of_instances(+C, -Count:nonneg) is det.

rdfs_number_of_instances(C, N) :-
  aggregate_all(count, rdfs_instance(_, C), N).



%! rdfs_number_of_properties(-Count:nonneg) is det.

rdfs_number_of_properties(N) :-
  aggregate_all(count, rdfs_property(_), N).
