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
    rdfs_label_value/4, % ?Term:rdf_term
                        % ?Label:atom
                        % ?LangTagPreference:list(list(atom))
                        % ?Graph:atom
    rdfs_reachable/3, % ?Subject:or([bnode,iri]),
                      % ?Predicate:iri,
                      % ?Object:rdf_term
    rdfs_subclass/2, % ?Subclass:iri
                     % ?Superclass:iri
    rdfs_subproperty/2 % ?Subproperty:iri
                       % ?Superproperty:iri
  ]
).

/** <module> RDF API: Read RDFS constructs

@author Wouter Beek
@version 2014/11-2014/12
*/

:- use_module(library(lists), except([delete/3])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(closure)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_subproperty_test)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_property(r)).
:- rdf_meta(rdf_property(r,?)).
:- rdf_meta(rdfs_class(r)).
:- rdf_meta(rdfs_class(r,?)).
:- rdf_meta(rdfs_datatype(r)).
:- rdf_meta(rdfs_datatype(r,?)).
:- rdf_meta(rdfs_has(r,r,o)).
:- rdf_meta(rdfs_instance(o,r)).
:- rdf_meta(rdfs_label(o,?,?,?)).
:- rdf_meta(rdfs_label(o,?,?,?)).
:- rdf_meta(rdfs_reachable(r,r,o)).
:- rdf_meta(rdfs_subclass(r,r)).
:- rdf_meta(rdfs_subproperty(r,r)).
% Private.
:- rdf_meta(rdf_class_term(r)).
:- rdf_meta(rdf_property_term(r)).
:- rdf_meta(rdfs_class_term(r)).
:- rdf_meta(rdfs_property_term(r)).





%! rdf_property(+Property:iri) is semidet.
%! rdf_property(-Property:iri) is multi.

rdf_property(Property):-
  rdfs_instance(Property, rdfs:'Property').

%! rdf_property(?Property:iri, ?Graph:atom) is nondet

rdf_property(Property, Graph):-
  rdf_property(Property),
  once(rdf_term(Property, Graph)).



%! rdfs_class(+Class:iri) is semidet.
%! rdfs_class(-Class:iri) is multi.

rdfs_class(Class):-
  rdfs_instance(Class, rdfs:'Class').

%! rdfs_class(?Class:iri, ?Graph:atom) is nondet

rdfs_class(Class, Graph):-
  rdfs_class(Class),
  once(rdf_term(Class, Graph)).



%! rdfs_datatype(+Datatype:iri) is semidet.
%! rdfs_datatype(-Datatype:iri) is multi.

rdfs_datatype(Datatype):-
  rdfs_instance(Datatype, rdfs:'Datatype').

%! rdfs_datatype(?Datatype:iri, ?Graph:atom) is nondet

rdfs_datatype(Datatype, Graph):-
  rdfs_datatype(Datatype),
  once(rdf_term(Datatype, Graph)).



%! rdfs_has(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term
%! ) is nondet.

rdfs_has(S, P, O):-
  rdfs_subproperty(P0, P),
  rdf(S, P0, O).



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
% [3e] Property: occurrence in the data.
rdfs_instance(Property, rdf:'Property'):-
  rdf_current_predicate(Property),
  \+ rdf_property_term(Property),
  \+ rdfs_property_term(Property).

% [4] Resource: IRIs.
rdfs_instance(Resource, rdfs:'Resource'):-
  rdf_resource(Resource).

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

% [6a] RDF list: RDF vocabulary.
% "no special semantic conditions are imposed on this vocabulary
%  other than the type of `rdf:nil` being `rdf:List`." [RDF 1.1 Semantics]
rdfs_instance(rdf:nil, rdf:'List').
% [6b] RDF list: RDFS vocabulary.
rdfs_instance(List, rdf:'List'):-
  rdf(List, rdf:first, _).
rdfs_instance(List, rdf:'List'):-
  rdf(List, rdf:rest, _).



%! rdfs_label(
%!   ?Term:rdf_term,
%!   ?Value,
%!   ?LangTagPreference:list(list(atom)),
%!   ?Graph:atom
%! ) is nondet.
% Reads RDFS labels attributed to resources.

rdfs_label(Term, Value, LangPrefs, Graph):-
  rdf_plain_literal(Term, rdfs:label, Value, LangPrefs, Graph).

%! rdfs_label_value(
%!   ?Term:rdf_term,
%!   ?Label:atom,
%!   ?LangTagPreference:list(list(atom)),
%!   ?Graph:atom
%! ) is nondet.
% Since RDFS labels can be of type `rdf:langTag` or `xsd:string`,
% the `Value` returned by rdfs_label/4 can be either an atom or a pair.
%
% This predicate normalizes argument Label.
% It is either the full `Value` (if `xsd:string`)
% or the second argument of the pair (if `rdf:langTag`).
%
% @see rdfs_labels/4

rdfs_label_value(Term, Label, LangPrefs, Graph):-
  rdfs_label(Term, Value, LangPrefs, Graph),
  rdfs_label_value(Value, Label).

rdfs_label_value(Label-_, Label):- !.
rdfs_label_value(Label, Label).



%! rdfs_reachable(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term
%! ) is nondet.

rdfs_reachable(S, P, O):-
  closure0(rdfs_has0(P), S, O).



%! rdfs_sublass(?Subclass:iri, ?Superclass:iri) is nondet.

rdfs_subclass(Class, Class):-
  rdfs_instance(Class, rdfs:'Class').
rdfs_subclass(C1, C2):-
  rdf_global_id(rdfs:subClassOf, P),
  (   nonvar(C1)
  ->  closure0(rdfs_has0(P), C1, C2)
  ;   closure0(rdfs_has_backward0(P), C1, C2)
  ).



%! rdfs_subproperty(+Subproperty:iri, +Superproperty:iri) is semidet.
%! rdfs_subproperty(+Subproperty:iri, -Superproperty:iri) is multi.
%! rdfs_subproperty(-Subproperty:iri, +Superproperty:iri) is multi.

% [1] Reflexive-transitive closure of the subproperty hierarchy.
rdfs_subproperty(P1, P2):-
  subproperty_closure(Qs),
  (   nonvar(P1)
  ->  closure0(rdfs_forward(Qs), P1, P2)
  ;   closure0(rdfs_backward(Qs), P2, P1)
  ).
% [2] Container membership properties.
% "If x is in ICEXT(I(rdfs:ContainerMembershipProperty)) then:
%  < x, I(rdfs:member) > is in IEXT(I(rdfs:subPropertyOf))"
% [RDF 1.1 Semantics]
rdfs_subproperty(P, rdfs:member):-
  rdfs_instance(P, rdfs:'ContainerMembershipProperty').

rdfs_forward(Qs, X, Y):-
  member(Q, Qs),
  rdf(X, Q, Y).

rdfs_backward(Qs, X, Y):-
  member(Q, Qs),
  rdf(Y, Q, X).





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



rdfs_has_backward0(P, S, O):-
  rdfs_subproperty(P0, P),
  rdf(O, P0, S).



rdfs_has0(P, S, O):-
  rdfs_has(S, P, O).



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
