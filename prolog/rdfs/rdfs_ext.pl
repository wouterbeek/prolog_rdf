:- module(
  rdfs_ext,
  [
    rdfs_class/1,    % ?C
    rdfs_instance/2, % ?I, ?C
    rdfs_property/1  % ?Prop
  ]
).

/** <module> RDFS extensions

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   rdfs_class(r),
   rdfs_instance(r, r),
   rdfs_property(r).





rdfs_class(C) :-
  distinct(C, rdfs_class0(C)).

rdfs_class0(C) :-
  rdfs_instance(C, rdfs:'Class').
rdfs_class0(C) :-
  rdfs_subclass_of(C, _).
rdfs_class0(C) :-
  rdfs_subclass_of(_, C).
rdfs_class0(C) :-
  rdf_has(_, rdfs:domain, C).
rdfs_class0(C) :-
  rdf_has(_, rdfs:range, C).
rdfs_class0(C) :-
  rdf_has(_, rdf:type, C).



rdfs_instance(I, C) :-
  rdfs_individual_of(I, C).



rdfs_property(Prop) :-
  distinct(Prop, rdfs_property0(Prop)).

rdfs_property0(Prop) :-
  rdf_predicate(Prop).
rdfs_property0(Prop) :-
  rdfs_instance(Prop, rdf:'Property').
rdfs_property0(Prop) :-
  rdfs_subproperty_of(Prop, _).
rdfs_property0(Prop) :-
  rdfs_subproperty_of(_, Prop).
