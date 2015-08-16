:- module(
  owl_build,
  [
    owl_assert_functional_property/2, % +Property:iri
                                      % ?Graph:atom
    owl_assert_object_property/2, % +Property:iri
                                  % ?Graph:atom
    owl_assert_ontology/2, % +Ontology:or([bnode,iri])
                           % ?Graph:atom
    owl_assert_value_restriction/4 % +Property:or([bnode,iri])
                                   % +Value:or([bnode,iri])
                                   % ?Graph:atom
                                   % -Restriction:bnode
  ]
).

:- reexport(library(rdf/rdf_build)).

/** <module> OWL build

Predicates for asserting common OWL structures.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(rdf/rdf_build)).

:- rdf_meta(owl_assert_functional_property(r,?)).
:- rdf_meta(owl_assert_object_property(r,?)).
:- rdf_meta(owl_assert_ontology(r,?)).
:- rdf_meta(owl_assert_value_restriction(r,r,?,-)).





%! owl_assert_functional_property(+Property:iri, ?Graph:atom) is det.

owl_assert_functional_property(I, G):-
  rdf_assert_instance(I, owl:'FunctionalProperty', G).



%! owl_assert_object_property(+Property:iri, ?Graph:atom) is det.

owl_assert_object_property(I, G):-
  rdf_assert_instance(I, owl:'ObjectProperty', G).



%! owl_assert_ontology(+Ontology:or([bnode,iri]), ?Graph:atom) is det.

owl_assert_ontology(I, G):-
  rdf_assert_instance(I, owl:'Ontology', G).



%! owl_assert_value_restriction(
%!   +Property:or([bnode,iri]),
%!   +Value:or([bnode,iri]),
%!   ?Graph:atom,
%!   -Restriction:bnode
%! ) is det.

owl_assert_value_restriction(P, V, G, R):-
  rdf_bnode(R),
  rdf_assert_instance(R, owl:'Restriction', G),
  rdf_assert2(R, owl:onProperty, P, G),
  rdf_assert2(R, owl:hasValue, V, G).
