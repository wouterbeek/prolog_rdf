:- module(
  owl_build,
  [
    owl_assert_class/5, % +Class:iri
                        % ?ParentClass:or([iri,list(iri)])
                        % ?Label:or([atom,pair(atom)])
                        % ?Comment:or([atom,pair(atom)])
                        % ?Graph:atom
    owl_assert_data_property/2, % +Property:iri
                                % ?Graph:atom
    owl_assert_data_property/8, % +Property:iri
                                % ?ParentProperty:or([iri,list(iri)])
                                % ?Label:or([atom,pair(atom)])
                                % ?Comment:or([atom,pair(atom)])
                                % ?Domain:or([bnode,iri])
                                % ?Range:or([bnode,iri])
                                % ?Graph:atom
                                % +Options:list(compound)
    owl_assert_equivalent_class/3, % +Class1:or([bnode,iri])
                                   % +Class2:or([bnode,iri])
                                   % ?Graph:atom
    owl_assert_functional_property/2, % +Property:iri
                                      % ?Graph:atom
    owl_assert_intersection_of/3, % +Class:or([bnode,iri])
                                  % +Classes:list(or([bnode,iri])
                                  % ?Graph:atom
    owl_assert_named_individual/5, % +Individual:iri
                                   % ?Class:or([iri,list(iri)])
                                   % ?Label:or([atom,pair(atom)])
                                   % ?Comment:or([atom,pair(atom)])
                                   % ?Graph:atom
    owl_assert_object_property/2, % +Property:iri
                                  % ?Graph:atom
    owl_assert_ontology/2, % +Ontology:or([bnode,iri])
                           % ?Graph:atom
    owl_assert_object_property/8, % +Property:iri
                                  % ?ParentProperty:or([iri,list(iri)])
                                  % ?Label:or([atom,pair(atom)])
                                  % ?Comment:or([atom,pair(atom)])
                                  % ?Domain:or([bnode,iri])
                                  % ?Range:or([bnode,iri])
                                  % ?Graph:atom
                                  % +Options:list(compound)
    owl_assert_value_restriction/4 % +Property:or([bnode,iri])
                                   % +Value:or([bnode,iri])
                                   % ?Graph:atom
                                   % -Restriction:bnode
  ]
).

/** <module> OWL build

Predicates for asserting common OWL structures.

@author Wouter Beek
@version 2015/08-2015/10
*/

:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdfs/rdfs_build)).

:- rdf_meta(owl_assert_class(r,t,?,?,?)).
:- rdf_meta(owl_assert_data_property(r,?)).
:- rdf_meta(owl_assert_data_property(r,t,?,?,r,r,?,+)).
:- rdf_meta(owl_assert_equivalent_class(r,r,?)).
:- rdf_meta(owl_assert_functional_property(r,?)).
:- rdf_meta(owl_assert_intersection_of(r,t,?)).
:- rdf_meta(owl_assert_named_individual(r,t,?,?,?)).
:- rdf_meta(owl_assert_object_property(r,?)).
:- rdf_meta(owl_assert_object_property(r,t,?,?,r,r,?,+)).
:- rdf_meta(owl_assert_ontology(r,?)).
:- rdf_meta(owl_assert_value_restriction(r,r,?,-)).

:- predicate_options(owl_assert_data_property/8, 8, [
     pass_to(owl_assert_property/8, 8)
   ]).
:- predicate_options(owl_assert_object_property/8, 8, [
     pass_to(owl_assert_property/8, 8)
   ]).
:- predicate_options(owl_assert_property/8, 8, [
     functional(+boolean)
   ]).





%! owl_assert_class(
%!   +Class:iri,
%!   ?Parent:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Graph:atom
%! ) is det.

owl_assert_class(C, Parent, Lbl, Comm, G):-
  rdf_assert_instance(C, owl:'Class', G),
  rdfs_build:assert_class(C, Parent, Lbl, Comm, G).



%! owl_assert_data_property(+Property:iri, ?Graph:atom) is det.

owl_assert_data_property(I, G):-
  rdf_assert_instance(I, owl:'DataProperty', G).

%! owl_assert_data_property(
%!   +Property:iri,
%!   ?ParentProperty:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Domain:or([bnode,iri]),
%!   ?Range:or([bnode,iri]),
%!   ?Graph:atom,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * functional(+boolean)

owl_assert_data_property(P, Parent, Lbl, Comm, D, R, G, Opts):-
  owl_assert_data_property(P, G),
  owl_assert_property(P, Parent, Lbl, Comm, D, R, G, Opts).



%! owl_assert_equivalent_class(
%!   +Class1:or([bnode,iri]),
%!   +Class2:or([bnode,iri]),
%!   ?Graph:atom
%! ) is det.

owl_assert_equivalent_class(C, D, G):-
  user:rdf_assert(C, owl:equivalentClass, D, G).



%! owl_assert_functional_property(+Property:iri, ?Graph:atom) is det.

owl_assert_functional_property(I, G):-
  rdf_assert_instance(I, owl:'FunctionalProperty', G).



%! owl_assert_intersection_of(
%!   ?Class:or([bnode,iri]),
%!   +Classes:list(or([bnode,iri])),
%!   ?Graph:atom
%! ) is det.

owl_assert_intersection_of(C, Ds, G):-
  (var(C) -> rdf_bnode(C) ; true),
  rdf_assert_instance(C, owl:'Class', G),
  rdf_assert_list(Ds, Ds0, G),
  user:rdf_assert(C, owl:intersectionOf, Ds0, G).



%! owl_assert_named_individual(
%!   +Individual:iri,
%!   ?Class:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Graph:atom
%! ) is det.

owl_assert_named_individual(I, C, Lbl, Comm, G):-
  rdf_assert_instance(I, owl:'NamedIndividual', G),
  rdf_assert_instance(I, C, G),
  (var(Lbl) -> true ; rdfs_assert_label(I, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(I, Comm, G)),
  rdfs_assert_isDefinedBy(I, G).



%! owl_assert_object_property(+Property:iri, ?Graph:atom) is det.

owl_assert_object_property(I, G):-
  rdf_assert_instance(I, owl:'ObjectProperty', G).

%! owl_assert_object_property(
%!   +Property:iri,
%!   ?ParentProperty:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Domain:or([bnode,iri]),
%!   ?Range:or([bnode,iri]),
%!   ?Graph:atom,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * functional(+boolean)

owl_assert_object_property(P, Parent, Lbl, Comm, D, R, G, Opts):-
  owl_assert_object_property(P, G),
  owl_assert_property(P, Parent, Lbl, Comm, D, R, G, Opts).



%! owl_assert_ontology(+Ontology:or([bnode,iri]), ?Graph:atom) is det.

owl_assert_ontology(I, G):-
  rdf_assert_instance(I, owl:'Ontology', G).



%! owl_assert_value_restriction(
%!   +Property:iri,
%!   +Value:or([bnode,iri]),
%!   ?Graph:atom,
%!   -Restriction:bnode
%! ) is det.

owl_assert_value_restriction(P, V, G, R):-
  rdf_bnode(R),
  rdf_assert_instance(R, owl:'Restriction', G),
  user:rdf_assert(R, owl:onProperty, P, G),
  user:rdf_assert(R, owl:hasValue, V, G).





% HELPERS %

%! owl_assert_property(
%!   +Property:iri,
%!   ?ParentProperty:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Domain:or([bnode,iri]),
%!   ?Range:or([bnode,iri]),
%!   ?Graph:atom,
%!   +Options:list(compound)
%! ) is det.

owl_assert_property(P, Parent, Lbl, Comm, D, R, G, Opts):-
  (   option(functional(true), Opts)
  ->  owl_assert_functional_property(P, G)
  ;   true
  ),
  (   var(Parent)
  ->  true
  ;   is_list(Parent)
  ->  maplist(\Parent0^rdfs_assert_subproperty(P, Parent0, G), Parent)
  ;   rdfs_assert_subproperty(P, Parent, G)
  ),
  (var(Lbl) -> true ; rdfs_assert_label(P, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(P, Comm, G)),
  (var(D) -> true ; rdfs_assert_domain(P, D, G)),
  (var(R) -> true ; rdfs_assert_range(P, R, G)),
  rdfs_assert_isDefinedBy(P, G).
