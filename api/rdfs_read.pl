:- module(
  rdfs_read,
  [
    rdfs_label/4, % ?Term:rdf_term
                  % ?Value
                  % ?LangTagPreference:list(list(atom))
                  % ?Graph:atom
    rdfs_label_value/2, % ?Term:rdf_term
                        % ?Label:atom
    rdfs_label_value/4 % ?Term:rdf_term
                       % ?Label:atom
                       % ?LangTagPreference:list(list(atom))
                       % ?Graph:atom
  ]
).

/** <module> RDFS Read API

@author Wouter Beek
@version 2014/11-2015/02
*/

:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdfs_label(o,?,?,?)).
:- rdf_meta(rdfs_label(o,?,?,?)).
:- rdf_meta(rdfs_label_value(o,?)).
:- rdf_meta(rdfs_label_value(o,?,?,?)).





%! rdfs_label(
%!   ?Term:rdf_term,
%!   ?Value,
%!   ?LangTagPreference:list(list(atom)),
%!   ?Graph:atom
%! ) is nondet.
% Reads RDFS labels attributed to resources.

rdfs_label(Term, Value, LangPrefs, Graph):-
  rdf_plain_literal(Term, rdfs:label, Value, LangPrefs, Graph).



%! rdfs_label_value(?Term:rdf_term, ?Label:atom) is nondet.

rdfs_label_value(Term, Label):-
  rdfs_label_value(Term, Label, _, _).

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
% @see rdfs_label/4

rdfs_label_value(Term, Label, LangPrefs, Graph):-
  rdfs_label(Term, Value, LangPrefs, Graph),
  (   Value = Label-_
  ->  true
  ;   Label = Value
  ).





% HELPERS %

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
