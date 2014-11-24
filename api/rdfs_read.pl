:- module(
  rdfs_read,
  [
    rdf_property/1, % ?Property:iri
    rdf_property/2, % ?Property:iri
                    % ?Graph:atom
    rdfs_class/1, % ?Class:iri
    rdfs_class/2, % ?Class:iri
                  % ?Graph:atom
    rdfs_datatype/1, % ?Datatype:iri
    rdfs_datatype/2, % ?Datatype:iri
                     % ?Graph:atom
    rdfs_instance/2, % ?Instance:rdf_term
                     % ?Class:iri
    rdfs_label/4, % ?Term:rdf_term
                  % ?Value
                  % ?LangTagPreference:list(list(atom))
                  % ?Graph:atom
    rdfs_sublass/2, % ?Subclass:iri
                    % ?Superclass:iri
    rdfs_subproperty/2 % ?Subproperty:iri
                       % ?Superproperty:iri
  ]
).

/** <module> RDF API: Read RDFS constructs

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_property(r)).
:- rdf_meta(rdf_property(r,?)).
:- rdf_meta(rdfs_class(r)).
:- rdf_meta(rdfs_class(r,?)).
:- rdf_meta(rdfs_datatype(r)).
:- rdf_meta(rdfs_datatype(r,?)).
:- rdf_meta(rdfs_instance(o,r)).
:- rdf_meta(rdfs_label(o,?,?,?)).
:- rdf_meta(rdfs_subclass(r,r)).
:- rdf_meta(rdfs_subproperty(r,r)).
% Private.
:- rdf_meta(rdf_class_term(r)).
:- rdf_meta(rdf_property_term(r)).
:- rdf_meta(rdfs_class_term(r)).
:- rdf_meta(rdfs_property_term(r)).





%! rdf_property(+Property:iri) is semidet.
%! rdf_property(-Property:iri) is multi.

rdf_property(Class):-
  rdfs_instance_of(Class, rdfs:'Class').

%! rdf_property(?Property:iri, ?Graph:atom) is nondet

rdf_property(Property, Graph):-
  rdfs_instance_of(Property, rdf:'Property'),
  once(rdf_term(Property, Graph)).



%! rdfs_class(+Class:iri) is semidet.
%! rdfs_class(-Class:iri) is multi.

rdfs_class(Class):-
  rdfs_instance_of(Class, rdfs:'Class').

%! rdfs_class(?Class:iri, ?Graph:atom) is nondet

rdfs_class(Class, Graph):-
  rdfs_class(Class),
  once(rdf_term(Class, Graph)).



%! rdfs_datatype(+Datatype:iri) is semidet.
%! rdfs_datatype(-Datatype:iri) is multi.

rdfs_datatype(Datatype):-
  rdfs_instance_of(Datatype, rdfs:'Datatype').

%! rdfs_datatype(?Datatype:iri, ?Graph:atom) is nondet

rdfs_datatype(Datatype, Graph):-
  rdfs_datatype(Datatype),
  once(rdf_term(Datatype, Graph)).



%! rdfs_instance(+Resource, +Class) is semidet.
%! rdfs_instance(+Resource, -Class) is nondet.
%! rdfs_instance(-Resource, +Class) is nondet.
% Generate resources belonging to a class   or  classes a resource
% belongs to. We assume everything at the `object' end of a triple
% is a class. A validator should confirm this property.
%
% rdfs_instance_of(+, -) does  not  exploit   domain  and  range
% properties, deriving that if rdf(R,  P,   _)  is  present R must
% satisfy the domain of P (and similar for range).
%
% There are a few hacks:
%   - Any resource is an individual of rdfs:Resource
%   - literal(_) is an individual of rdfs:Literal

% [1a] Datatypes: required.
% "The datatype IRIs `rdf:langString` and `xsd:string` MUST be recognized
%  by all RDF interpretations." [RDF 1.1 Semantics]
rdfs_instance(rdf:langString, rdfs:'Datatype').
rdfs_instance(xsd:string, rdfs:'Datatype').

% [1b] Datatypes: optional.
% "Two other datatypes `rdf:XMLLiteral` and `rdf:HTML` are defined in
%  [RDF11-CONCEPTS]. RDF-D interpretations MAY fail to recognize these
%  datatypes." [RDF 1.1 Semantics]
rdfs_instance(rdf:'XMLLiteral', rdfs:'Datatype').
rdfs_instance(rdf:'HTML', rdfs:'Datatype').

% [1c] Datatypes: occurring in the data.
% "for every IRI aaa in D, I(aaa) is in ICEXT(I(rdfs:Datatype))"
% "When using RDFS semantics, the referents of all recognized datatype IRIs
%  can be considered to be in the class rdfs:Datatype." [RDF 1.1 Semantics]
rdfs_instance(Datatype, rdfs:'Datatype'):-
  rdf_current_literal(literal(type(Datatype,_))),
  \+ rdf_equal(Datatype, rdf:langString),
  \+ rdf_equal(Datatype, xsd:string),
  \+ rdf_equal(Datatype, rdf:'XMLLiteral'),
  \+ rdf_equal(Datatype, rdf:'HTML').

% [2] Datatyped values.
rdfs_instance(literal(type(Datatype,LexicalForm)), Datatype):-
  rdf_current_literal(type(Datatype,LexicalForm)).

% [3a] Property: RDF vocabulary.
rdfs_instance(Property, rdf:'Property'):-
  rdf_property_term(Property).
% [3b] Property: RDFS vocabulary.
rdfs_instance(Property, rdf:'Property'):-
  rdfs_property_term(Property).
% [3c] Property: RDFS vocabulary deduction.
rdfs_instance(Property, rdf:'Property'):-
  rdf(_, Property, _).
% [3d] Property: RDFS vocabulary deduction.
rdfs_instance(Property, rdf:'Property'):-
  rdf_has(Property, rdfs:subPropertyOf, _).
rdfs_instance(Property, rdf:'Property'):-
  rdf_has(_, rdfs:subPropertyOf, Property).
rdfs_instance(Property, rdf:'Property'):-
  rdf:type rdfs:ContainerMembershipProperty
% [3e] Property: occurrence in the data.
rdfs_instance(Property, rdf:'Property'):-
  rdf_current_property(Property),
  \+ rdf_property_term(Property),
  \+ rdfs_property_term(Property).

% [4] Resource: IRIs.
rdfs_instance(Resource, rdfs:'Resource'):-
  rdf_resource(Resource).
rdfs_instance(Resource, rdfs:'Resource'):-

% [5a] Class: RDF vocabulary.
rdfs_instance(Class, rdfs:'Class'):-
  rdf_class_term(Class).
% [5b] Class: RDFS vocabulary.
rdfs_instance(Class, rdfs:'Class'):-
  rdfs_class_term(Class).
% [5c] Class: RDF vocabulary deduction.
rdfs_instance(Class, rdfs:'Class'):-
  rdf_has(_, rdf:type, Class).
% [5d] Class: RDFS vocabulary deduction.
rdfs_instance(Class, rdfs:'Class'):-
  rdf_has(Class, rdf:type, rdfs:'Datatype').
rdfs_instance(Class, rdfs:'Class'):-
  rdf_has(_, rdfs:domain, Class).
rdfs_instance(Class, rdfs:'Class'):-
  rdf_has(_, rdfs:range, Class).
rdfs_instance(Class, rdfs:'Class'):-
  rdf_has(Class, rdfs:subClassOf, _).
rdfs_instance(Class, rdfs:'Class'):-
  rdf_has(_, rdfs:subClassOf, Class).

% [6] RDF list.
% "no special semantic conditions are imposed on this vocabulary
%  other than the type of `rdf:nil` being `rdf:List`." [RDF 1.1 Semantics]
rdfs_instance(rdf:nil, rdf:'List').



%! rdfs_label(
%!   ?Term:rdf_term,
%!   ?Value,
%!   ?LangTagPreference:list(list(atom)),
%!   ?Graph:atom
%! ) is nondet.
% Reads RDFS labels attributed to resources.

rdfs_label(Term, Value, LangTags, Graph):-
  rdf_plain_literal(Term, rdfs:label, Value, LangTags, Graph).





% HELPERS

%! is_membership_property(+Property:iri) is semidet
% Succeeds if the given IRI denotes an RDF membership property.

is_membership_property(MembershipProperty):-
  rdf_global_id(rdf:LocalName, MembershipProperty),
  atom_concat('_', N, LocalName),
  between(1, inf, N).



%! rdf_class_term(+Class:iri) is semidet.
%! rdf_class_term(-Class:iri) is multi.

rdf_class_term(rdf:'Alt').
rdf_class_term(rdf:'Bag').
rdf_class_term(rdf:'List').
rdf_class_term(rdf:'Property').
rdf_class_term(rdf:'Seq').
rdf_class_term(rdf:'Statement').



%! rdf_property_term(+Property:iri) is semidet.
%! rdf_property_term(-Property:iri) is multi.

rdf_property_term(MembershipProperty):-
  (   var(MembershipProperty)
  ->  between(1, inf, N),
      atom_concat('_', N, LocalName),
      rdf_global_id(rdf:LocalName, MembershipProperty)
  ;   is_membership_property(MembershipProperty)
  ).
rdf_property_term(rdf:first).
rdf_property_term(rdf:object).
rdf_property_term(rdf:predicate).
rdf_property_term(rdf:rest).
rdf_property_term(rdf:subject).
rdf_property_term(rdf:type).
rdf_property_term(rdf:value).



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
