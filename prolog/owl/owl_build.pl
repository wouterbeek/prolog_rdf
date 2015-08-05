:- module(
  owl_build,
  [
    owl_assert_value_restriction/4 % +Property:iri
                                   % +Value:iri
                                   % ?Graph:atom
                                   % -Restriction:iri
  ]
).

:- reexport(library(rdf/rdf_build)).

/** <module> OWL build

Predicates for asserting common OWL structures.

@author Wouter Beek
@version 2015/08
*/

:- rdf_meta(owl_assert_value_restriction(r,r,?,-)).





%! owl_assert_value_restriction(
%!   +Property:iri,
%!   +Value:iri,
%!   ?Graph:atom,
%!   -Restriction:iri
%! ) is det.

owl_assert_value_restriction(P, V, G, R):-
  rdf_bnode(R),
  rdf_assert_instance(R, owl:'Restriction', G),
  rdf_assert2(R, owl:onProperty, P, G),
  rdf_assert2(R, owl:hasValue, V, G).
