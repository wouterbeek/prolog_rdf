:- module(
  owl_build,
  [
    owl_assert_class/5, % +Class:iri
                        % ?ParentClass:or([iri,list(iri)])
                        % ?Label:or([atom,pair(atom)])
                        % ?Comment:or([atom,pair(atom)])
                        % ?Graph:rdf_graph
    owl_assert_data_property/2, % +Property:iri
                                % ?Graph:rdf_graph
    owl_assert_data_property/8, % +Property:iri
                                % ?ParentProperties:or([iri,list(iri)])
                                % ?Label:or([atom,pair(atom)])
                                % ?Comment:or([atom,pair(atom)])
                                % ?Domain:rdf_term
                                % ?Range:rdf_term
                                % ?Graph:rdf_graph
                                % +Options:list(compound)
    owl_assert_disjointWith/3, % +Class1:rdf_term
                               % +Class2:rdf_term
                               % ?Graph:rdf_graph
    owl_assert_equivalent_class/3, % +Class1:rdf_term
                                   % +Class2:rdf_term
                                   % ?Graph:rdf_graph
    owl_assert_functional_property/2, % +Property:iri
                                      % ?Graph:rdf_graph
    owl_assert_intersection_of/3, % +Class:rdf_term
                                  % +Classes:list(rdf_term
                                  % ?Graph:rdf_graph
    owl_assert_list/2, % +PrologList:list(rdf_term)
                       % ?RdfList:rdf_term
    owl_assert_literal/4, % +Subject:rdf_term
                          % +Predicate:rdf_term
                          % ?Datatype:iri
                          % +Value
    owl_assert_named_individual/5, % +Individual:iri
                                   % ?Class:or([iri,list(iri)])
                                   % ?Label:or([atom,pair(atom)])
                                   % ?Comment:or([atom,pair(atom)])
                                   % ?Graph:rdf_graph
    owl_assert_object_property/2, % +Property:iri
                                  % ?Graph:rdf_graph
    owl_assert_object_property/8, % +Property:iri
                                  % ?ParentProperties:or([iri,list(iri)])
                                  % ?Label:or([atom,pair(atom)])
                                  % ?Comment:or([atom,pair(atom)])
                                  % ?Domain:rdf_term
                                  % ?Range:rdf_term
                                  % ?Graph:rdf_graph
                                  % +Options:list(compound)
    owl_assert_ontology/2, % +Ontology:rdf_term
                           % ?Graph:rdf_graph
    owl_assert_value_restriction/4 % +Property:iri
                                   % +Value:rdf_term
                                   % ?Graph:rdf_graph
                                   % -Restriction:rdf_bnode
  ]
).

/** <module> OWL build

Predicates for asserting common OWL structures.

@author Wouter Beek
@version 2015/08-2015/10, 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_database)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdfs/rdfs_build)).

:- rdf_meta(owl_assert_class(r,t,?,?,?)).
:- rdf_meta(owl_assert_data_property(r,?)).
:- rdf_meta(owl_assert_data_property(r,t,?,?,r,r,?,+)).
:- rdf_meta(owl_assert_disjointWith(o,o,r)).
:- rdf_meta(owl_assert_equivalent_class(r,r,?)).
:- rdf_meta(owl_assert_functional_property(r,?)).
:- rdf_meta(owl_assert_intersection_of(r,t,?)).
:- rdf_meta(owl_assert_list(t,o)).
:- rdf_meta(owl_assert_literal(o,o,o,+)).
:- rdf_meta(owl_assert_named_individual(r,t,?,?,?)).
:- rdf_meta(owl_assert_object_property(r,r)).
:- rdf_meta(owl_assert_object_property(r,t,?,?,r,r,r,+)).
:- rdf_meta(owl_assert_ontology(r,r)).
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
%!   ?Graph:rdf_graph
%! ) is det.

owl_assert_class(C, Parent, Lbl, Comm, G):-
  rdf_assert_instance(C, owl:'Class', G),
  rdfs_build:rdfs_assert_class0(C, Parent, Lbl, Comm, G).



%! owl_assert_data_property(+Property:iri, ?Graph:rdf_graph) is det.

owl_assert_data_property(I, G):-
  rdf_assert_instance(I, owl:'DataProperty', G).


%! owl_assert_data_property(
%!   +Property:iri,
%!   ?ParentProperty:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Domain:rdf_term,
%!   ?Range:rdf_term,
%!   ?Graph:rdf_graph,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * functional(+boolean)

owl_assert_data_property(P, Parent, Lbl, Comm, D, R, G, Opts):-
  owl_assert_data_property(P, G),
  owl_assert_property(P, Parent, Lbl, Comm, D, R, G, Opts).



%! owl_assert_disjointWith(
%!   +Class1:rdf_term,
%!   +Class2:rdf_term,
%!   ?Graph:rdf_graph
%! ) is det.

owl_assert_disjointWith(C1, C2, G):-
  rdf_assert(C1, owl:disjointWith, C2, G).



%! owl_assert_equivalent_class(
%!   +Class1:rdf_term,
%!   +Class2:rdf_term,
%!   ?Graph:rdf_graph
%! ) is det.

owl_assert_equivalent_class(C, D, G):-
  rdf_assert(C, owl:equivalentClass, D, G).



%! owl_assert_functional_property(+Property:iri, ?Graph:rdf_graph) is det.

owl_assert_functional_property(I, G):-
  rdf_assert_instance(I, owl:'FunctionalProperty', G).



%! owl_assert_intersection_of(
%!   ?Class:rdf_term,
%!   +Classes:list(rdf_term),
%!   ?Graph:rdf_graph
%! ) is det.

owl_assert_intersection_of(C, Ds, G):-
  (var(C) -> rdf_bnode(C) ; true),
  rdf_assert_instance(C, owl:'Class', G),
  rdf_assert_list(Ds, Ds0, G),
  rdf_assert(C, owl:intersectionOf, Ds0, G).



%! owl_assert_list(+PrologList:list(rdf_term), ?RdfList:rdf_term) is det.
% Asserts the given, possibly nested, list into RDF.

owl_assert_list(L1, L2):-
  rdf_transaction(owl_assert_list0(L1, L2)).

owl_assert_list0([], rdf:nil):- !.
owl_assert_list0(L1, L2):-
  owl_add_list_instance0(L2),
  owl_assert_list_items0(L1, L2).

owl_assert_list_items0([], rdf:nil):- !.
owl_assert_list_items0([H1|T1], L2):-
  % rdf:first
  (   % Nested list.
      is_list(H1)
  ->  owl_assert_list0(H1, H2)
  ;   % Non-nested list.
      H2 = H1
  ),
  rdf_assert(L2, rdf:first, H2),

  % rdf:rest
  (   T1 == []
  ->  rdf_expand_ct(rdf:nil, T2)
  ;   owl_add_list_instance0(T2),
      owl_assert_list_items0(T1, T2)
  ),
  rdf_assert(L2, rdf:rest, T2).

owl_add_list_instance0(L):-
  (var(L) -> rdf_bnode(L) ; true),
  rdf_assert_instance(L, rdf:'List').



%! owl_assert_literal(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   ?Datatype:iri,
%!   +Value
%! ) is det.

% Language-tagged strings.
owl_assert_literal(S, P, rdf:langString, Lex-LTag):- !,
  rdf_assert(S, P, literal(lang(LTag,Lex))).
% Simple literals (as per RDF 1.0 specification)
% assumed to be of type `xsd:string` (as per RDF 1.1 specification).
owl_assert_literal(S, P, D, Val):-
  var(D), !,
  rdf_assert_literal(S, P, xsd:string, Val).
% Typed literals (as per RDF 1.0 specification).
owl_assert_literal(S, P, D, Val):-
  rdf_canonical_map(D, Val, Lit),
  rdf_assert(S, P, Lit).



%! owl_assert_named_individual(
%!   +Individual:iri,
%!   ?Class:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Graph:rdf_graph
%! ) is det.

owl_assert_named_individual(I, C, Lbl, Comm, G):-
  rdf_assert_instance(I, owl:'NamedIndividual', G),
  rdf_assert_instance(I, C, G),
  (var(Lbl) -> true ; rdfs_assert_label(I, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(I, Comm, G)),
  rdfs_assert_isDefinedBy(I, G).



%! owl_assert_object_property(+Property:iri, ?Graph:rdf_graph) is det.

owl_assert_object_property(I, G):-
  rdf_assert_instance(I, owl:'ObjectProperty', G).


%! owl_assert_object_property(
%!   +Property:iri,
%!   ?ParentProperties:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Domain:rdf_term,
%!   ?Range:rdf_term,
%!   ?Graph:rdf_graph,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * functional(+boolean)

owl_assert_object_property(P, Parent, Lbl, Comm, D, R, G, Opts):-
  owl_assert_object_property(P, G),
  owl_assert_property(P, Parent, Lbl, Comm, D, R, G, Opts).



%! owl_assert_ontology(+Ontology:rdf_term, ?Graph:rdf_graph) is det.

owl_assert_ontology(I, G):-
  rdf_assert_instance(I, owl:'Ontology', G).



%! owl_assert_value_restriction(
%!   +Property:iri,
%!   +Value:rdf_term,
%!   ?Graph:rdf_graph,
%!   -Restriction:rdf_bnode
%! ) is det.

owl_assert_value_restriction(P, V, G, R):-
  rdf_create_bnode(R),
  rdf_assert_instance(R, owl:'Restriction', G),
  rdf_assert(R, owl:onProperty, P, G),
  rdf_assert(R, owl:hasValue, V, G).





% HELPERS %

%! owl_assert_property(
%!   +Property:iri,
%!   ?ParentProperties:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Domain:rdf_term,
%!   ?Range:rdf_term
%!   ?Graph:rdf_graph,
%!   +Options:list(compound)
%! ) is det.

owl_assert_property(P, Parents, Lbl, Comm, D, R, G, Opts):-
  (   option(functional(true), Opts)
  ->  owl_assert_functional_property(P, G)
  ;   true
  ),
  (   var(Parents)
  ->  true
  ;   is_list(Parents)
  ->  maplist(rdfs_assert_subproperty0(P, G), Parents)
  ;   rdfs_assert_subproperty(P, Parents, G)
  ),
  (var(Lbl) -> true ; rdfs_assert_label(P, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(P, Comm, G)),
  (var(D) -> true ; rdfs_assert_domain(P, D, G)),
  (var(R) -> true ; rdfs_assert_range(P, R, G)),
  rdfs_assert_isDefinedBy(P, G).

rdfs_assert_subproperty0(P, G, Parent):- rdfs_assert_subproperty(P, Parent, G).
