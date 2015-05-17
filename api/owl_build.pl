:- module(
  owl_build,
  [
    owl_assert_class_equivalence/3, % +Class1:class
                                    % +Class2:class
                                    % +Graph:atom
    owl_assert_disjointWith/2, % +Classes:list(iri)
                               % +Graph:atom
    owl_assert_disjointWith/3, % +Class1:iri
                               % +Class2:iri
                               % +Graph:atom
    owl_assert_resource_identity/3, % +Resource1:class
                                    % +Resource2:class
                                    % +Graph:atom
    owl_retractall_class_equivalence/3, % +Class1:class
                                        % +Class2:class
                                        % +Graph:atom
    owl_retractall_resource_identity/3 % +Resource1:class
                                       % +Resource2:class
                                       % +Graph:atom
  ]
).

/** <module> OWL build

Predicates for building OWL ontologies.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03, 2013/05, 2013/09, 2014/12
*/

:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- rdf_meta(owl_assert_class_equivalence(r,r,+)).
:- rdf_meta(owl_assert_disjointWith(t,+)).
:- rdf_meta(owl_assert_disjointWith(r,r,+)).
:- rdf_meta(owl_assert_resource_identity(r,r,+)).
:- rdf_meta(owl_retractall_class_equivalence(r,r,+)).
:- rdf_meta(owl_retractall_resource_identity(r,r,+)).





%! owl_assert_class_equivalence(+Class1:iri, +Class2:iri, +Graph:atom) is det.

owl_assert_class_equivalence(C1, C2, G):-
  rdf_assert(C1, owl:equivalentClass, C2, G).



%! owl_assert_disjointWith(+Classes:list(iri), +Graph:atom) is det.

owl_assert_disjointWith([], _).
owl_assert_disjointWith([H|T], G):-
  maplist(\C^owl_assert_disjointWith(H, C, G), T),
  owl_assert_disjointWith(T, G).



%! owl_assert_disjointWith(+Class1:iri, +Class2:iri, +Graph:atom) is det.

owl_assert_disjointWith(C1, C2, G):-
  rdf_assert(C1, owl:disjointWith, C2, G).



%! owl_assert_resource_identity(
%!   +Resource1:iri,
%!   +Resource2:iri,
%!   +Graph:atom
%! ) is det.

owl_assert_resource_identity(Resource1, Resource2, Graph):-
  rdf_assert(Resource1, owl:sameAs, Resource2, Graph).



%! owl_retractall_class_equivalence(
%!   +Class1:iri,
%!   +Class2:iri,
%!   +Graph:atom
%! ) is det.

owl_retractall_class_equivalence(Class1, Class2, Graph):-
  rdf_retractall(Class1, owl:equivalentClass, Class2, Graph).



%! owl_retractall_resource_identity(
%!   +Resource1:iri,
%!   +Resource2:iri,
%!   +Graph:atom
%! ) is det.

owl_retractall_resource_identity(Resource1, Resource2, Graph):-
  rdf_retractall(Resource1, owl:sameAs, Resource2, Graph).

