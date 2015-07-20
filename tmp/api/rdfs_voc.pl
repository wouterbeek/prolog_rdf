:- module(
  rdfs_voc,
  [
    rdfs_class_term/1, % ?Class:iri
    rdfs_property_term/1 % ?Property:iri
  ]
).

/** <module> RDFS: vocabulary API

API access to terms in the RDFS vocabulary.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- rdf_meta(rdfs_class_term(r)).
:- rdf_meta(rdfs_property_term(r)).





%! rdfs_class_term(+Class:iri) is semidet.
%! rdfs_class_term(-Class:iri) is multi.

rdfs_class_term(rdfs:'Class').
rdfs_class_term(rdfs:'Container').
rdfs_class_term(rdfs:'ContainerMembershipProperty').
rdfs_class_term(rdfs:'Datatype').
rdfs_class_term(rdfs:'Literal').
rdfs_class_term(rdfs:'Resource').



%! rdfs_property_term(+Property:iri) is semidet.
%! rdfs_property_term(-Property:iri) is multi.

rdfs_property_term(rdfs:comment).
rdfs_property_term(rdfs:domain).
rdfs_property_term(rdfs:isDefinedBy).
rdfs_property_term(rdfs:label).
rdfs_property_term(rdfs:member).
rdfs_property_term(rdfs:range).
rdfs_property_term(rdfs:seeAlso).
rdfs_property_term(rdfs:subClassOf).
rdfs_property_term(rdfs:subPropertyOf).
