:- module(
  rdf_voc,
  [
    rdf_class_term/1, % ?Class:iri
    rdf_membership_property_term/1, % ?MembershipProperty:iri
    rdf_property_term/1 % ?Property:iri
  ]
).

/** <module> RDF: vocabulary API

API access to terms in the RDF vocabulary.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- rdf_meta(rdf_class_term(r)).
:- rdf_meta(rdf_membership_property_term(r)).
:- rdf_meta(rdf_property_term(r)).





%! rdf_class_term(+Class:iri) is semidet.
%! rdf_class_term(-Class:iri) is multi.

rdf_class_term(rdf:'Alt').
rdf_class_term(rdf:'Bag').
rdf_class_term(rdf:'List').
rdf_class_term(rdf:'Property').
rdf_class_term(rdf:'Seq').
rdf_class_term(rdf:'Statement').



%! rdf_membership_property_term(+MembershipProperty:iri) is semidet.
%! rdf_membership_property_term(-MembershipProperty:iri) is nondet

rdf_membership_property_term(MembershipProperty):-
  var(MembershipProperty), !,
  between(1, inf, N),
  atom_concat('_', N, LocalName),
  rdf_global_id(rdf:LocalName, MembershipProperty).
rdf_membership_property_term(MembershipProperty):-
  rdf_global_id(rdf:LocalName, MembershipProperty),
  atom_concat('_', N, LocalName),
  between(1, inf, N).



%! rdf_property_term(+Property:iri) is semidet.
%! rdf_property_term(-Property:iri) is multi.

rdf_property_term(rdf:first).
rdf_property_term(rdf:object).
rdf_property_term(rdf:predicate).
rdf_property_term(rdf:rest).
rdf_property_term(rdf:subject).
rdf_property_term(rdf:type).
rdf_property_term(rdf:value).
rdf_property_term(MembershipProperty):-
  rdf_membership_property_term(MembershipProperty).
