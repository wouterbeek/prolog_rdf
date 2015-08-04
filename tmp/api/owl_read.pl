:- module(
  owl_read,
  [
    owl_class_equiv/2, % ?Class1:iri
                       % ?Class2:iri
    owl_class_disjoint/2, % ?Class1:iri
                          % ?Class2:iri
    owl_ids/2 % +Term:iri
              % -IdTerms:ordset(iri)
  ]
).

/** <module> OWL read

Predicates for reading from OWL data.

@author Wouter Beek
@version 2013-2015
*/

:- use_module(library(aggregate)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta(owl_class_equiv(r,r)).
:- rdf_meta(owl_class_disjoint(r,r)).
:- rdf_meta(owl_ids(r,-)).





%! owl_class_equiv(+Class1:iri, +Class2:iri) is semidet.
%! owl_class_equiv(+Class1:iri, -Class2:iri) is nondet.
%! owl_class_equiv(-Class1:iri, +Class2:iri) is nondet.

owl_class_equiv(C1, C2):-
  rdf_reachable(C1, owl:equivalentClass, C2).



%! owl_class_disjoint(+Class1:iri, +Class2:iri) is semidet.
%! owl_class_disjoint(+Class1:iri, -Class2:iri) is nondet.
%! owl_class_disjoint(-Class1:iri, +Class2:iri) is nondet.

owl_class_disjoint(C1, C2):-
  rdf_reachable(C1, owl:disjointWith, C2).



%! owl_ids(+Term:iri, -IdenticalTerms:ordset(iri)) is det.
% Returns the identity set for the given IRI.

owl_ids(T0, ClT):-
  aggregate_all(set(T), owl_id(T0, T), ClT).
