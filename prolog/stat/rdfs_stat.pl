:- module(
  rdfs_stat,
  [
    rdfs_number_of_classes/1,    % -NumCs
    rdfs_number_of_classes/2,    % ?G, -NumCs
    rdfs_number_of_instances/2,  % +C, -NumIs
    rdfs_number_of_properties/1, % -NumProps
    rdfs_number_of_properties/2  % ?G, -NumProps
  ]
).

/** <module> RDFS statistics

@author Wouter Beek
@version 2015/12-2016/01, 2016/05-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdfs_number_of_classes(r, -),
   rdfs_number_of_instances(r, -),
   rdfs_number_of_properties(r, -).





%! rdfs_number_of_classes(-NumCs) is det.
%! rdfs_number_of_classes(?G, -NumCs) is det.

rdfs_number_of_classes(NumCs) :-
  rdfs_number_of_classes(_, NumCs).


rdfs_number_of_classes(G, NumCs) :-
  aggregate_all(count, rdfs_class(_, G), NumCs).



%! rdfs_number_of_instances(+C, -NumIs) is det.

rdfs_number_of_instances(C, NumIs) :-
  aggregate_all(count, rdfs_instance(_, C), NumIs).



%! rdfs_number_of_properties(-NumProps) is det.
%! rdfs_number_of_properties(?G, -NumProps) is det.

rdfs_number_of_properties(NumProps) :-
  rdfs_number_of_properties(_, NumProps).


rdfs_number_of_properties(G, NumProps) :-
  aggregate_all(count, rdfs_property(_, G), NumProps).
