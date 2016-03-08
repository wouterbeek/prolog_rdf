:- module(
  rdf_build,
  [
    owl_assert_class/5,               % +C, ?D, ?Label, ?Comment, ?G
    owl_assert_data_property/2,       % +P, ?G
    owl_assert_data_property/8,       % +P, ?Q, ?Label, ?Comment, ?Domain, ?Range, ?G, +Opts
    owl_assert_disjointWith/3,        % +C, +D, ?G
    owl_assert_equivalent_class/3,    % +C, +D, ?G
    owl_assert_functional_property/2, % +P, ?G
    owl_assert_intersection_of/3,     % +C, +Ds, ?G
    owl_assert_named_individual/5,    % +I, ?C, ?Label, ?Comment, ?G
    owl_assert_object_property/2,     % +P, ?G
    owl_assert_object_property/8,     % +P, ?Q, ?Label, ?Comment, ?Domain, ?Range, ?G, +Opts
    owl_assert_ontology/2,            % +Ontology, ?G
    owl_assert_value_restriction/4,   % +P, +Value, ?G, -Restriction
    rdf_assert_action/3,              % +ActionClass, +Actor, -Action
    rdf_assert_action/4,              % +ActionClass, +Actor, -Action, +G
    rdf_assert_instance/3,            % +I, +Cs, +G
    rdf_assert_now/2,                 % +S, +P
    rdf_assert_now/3,                 % +S, +P, +D
    rdf_assert_now/4,                 % +S, +P, +D, +G
    rdf_create_iri/2,                 % +Prefix, -Iri
    rdf_create_iri/3,                 % +Prefix, +SubPaths, -Iri
    rdfs_assert_class/5,              % +C, ?D, ?Label, ?Comment, ?G
    rdfs_assert_comment/3,            % +S, +Comment, ?G
    rdfs_assert_domain/3,             % +P, +C, ?G
    rdfs_assert_isDefinedBy/2,        % +S, ?G
    rdfs_assert_isDefinedBy/3,        % +S, ?Iri, ?G
    rdfs_assert_label/2,              % +S, +Label
    rdfs_assert_label/3,              % +S, +Label, ?G
    rdfs_assert_property/4,           % +C, +P, +D, ?G
    rdfs_assert_range/3,              % +P, +C, ?G
    rdfs_assert_seeAlso/3,            % +S, +Uri, +G
    rdfs_assert_subclass/2,           % +C, ?G
    rdfs_assert_subclass/3,           % +C, ?D, ?G
    rdfs_assert_subproperty/3,        % +P, +Q, ?G
    rdfs_retractall_class/1           % +C
  ]
).

/** <module> RDF: Build data structures

Predicates for asseritng RDFS statements in an easy way.

@author Wouter Beek
@version 2015/07-2015/09, 2015/12-2016/01
*/

:- use_module(library(apply)).
:- use_module(library(default)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- predicate_options(owl_assert_data_property/8, 8, [
     pass_to(owl_assert_property/8, 8)
   ]).
:- predicate_options(owl_assert_object_property/8, 8, [
     pass_to(owl_assert_property/8, 8)
   ]).
:- predicate_options(owl_assert_property/8, 8, [
     functional(+boolean)
   ]).

:- rdf_meta
   owl_assert_class(r, t, ?, ?, r),
   owl_assert_data_property(r, r),
   owl_assert_data_property(r, t, ?, ?, r, r, r, +),
   owl_assert_disjointWith(o, o, r),
   owl_assert_equivalent_class(r, r, r),
   owl_assert_functional_property(r, r),
   owl_assert_intersection_of(r, t, r),
   owl_assert_named_individual(r, t, ?, ?, r),
   owl_assert_object_property(r, r),
   owl_assert_object_property(r, t, ?, ?, r, r, r, +),
   owl_assert_ontology(r, r),
   owl_assert_value_restriction(r, r, r, -),
   rdf_assert_action(r, r, -),
   rdf_assert_action(r, r, -, r),
   rdf_assert_instance(r, t, r),
   rdf_assert_now(o, r),
   rdf_assert_now(o, r, r),
   rdf_assert_now(o, r, r, r),
   rdfs_assert_class(r, t, ?, ?, r),
   rdfs_assert_comment(r, +, r),
   rdfs_assert_domain(r, r, r),
   rdfs_assert_isDefinedBy(r, r),
   rdfs_assert_isDefinedBy(r, r, r),
   rdfs_assert_label(r, +),
   rdfs_assert_label(r, +, r),
   rdfs_assert_property(r, r, r, r),
   rdfs_assert_range(r, r, r),
   rdfs_assert_seeAlso(r, +, r),
   rdfs_assert_subclass(r, r),
   rdfs_assert_subclass(r, t, r),
   rdfs_assert_subproperty(r, t, r),
   rdfs_retractall_class(r).





%! owl_assert_class(+C, ?D, ?Label, ?Comment, ?G) is det.

owl_assert_class(C, D, Lbl, Comm, G) :-
  rdf_assert_instance(C, owl:'Class', G),
  rdfs_assert_class0(C, D, Lbl, Comm, G).



%! owl_assert_data_property(+P, ?G) is det.

owl_assert_data_property(P, G) :-
  rdf_assert_instance(P, owl:'DataProperty', G).


%! owl_assert_data_property(+P, ?Q, ?Label, ?Comment, ?Domain, ?Range, ?G, +Opts) is det.
% The following options are supported:
%   * functional(+boolean)

owl_assert_data_property(P, Q, Lbl, Comm, Domain, Range, G, Opts) :-
  owl_assert_data_property(P, G),
  owl_assert_property(P, Q, Lbl, Comm, Domain, Range, G, Opts).



%! owl_assert_disjointWith(+C, +D, ?G) is det.

owl_assert_disjointWith(C, D, G) :-
  rdf_assert(C, owl:disjointWith, D, G).



%! owl_assert_equivalent_class(+C, +D, ?G) is det.

owl_assert_equivalent_class(C, D, G) :-
  rdf_assert(C, owl:equivalentClass, D, G).



%! owl_assert_functional_property(+P, ?G) is det.

owl_assert_functional_property(P, G) :-
  rdf_assert_instance(P, owl:'FunctionalProperty', G).



%! owl_assert_intersection_of(?C, +Ds, ?G) is det.

owl_assert_intersection_of(C, Ds, G) :-
  defgoal(rdf_create_bnode, C),
  rdf_assert_instance(C, owl:'Class', G),
  rdf_assert_list(Ds, Ds0, G),
  rdf_assert(C, owl:intersectionOf, Ds0, G).



%! owl_assert_named_individual(+I, ?C, ?Label, ?Comment, ?G) is det.

owl_assert_named_individual(I, C, Lbl, Comm, G) :-
  rdf_assert_instance(I, owl:'NamedIndividual', G),
  rdf_assert_instance(I, C, G),
  (var(Lbl) -> true ; rdfs_assert_label(I, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(I, Comm, G)),
  rdfs_assert_isDefinedBy(I, G).



%! owl_assert_object_property(+P, ?G) is det.

owl_assert_object_property(I, G) :-
  rdf_assert_instance(I, owl:'ObjectProperty', G).


%! owl_assert_object_property(+P, ?Q, ?Label, ?Comment, ?Domain, ?Range, ?G, +Opts) is det.
% The following options are supported:
%   * functional(+boolean)

owl_assert_object_property(P, Parent, Lbl, Comm, D, R, G, Opts) :-
  owl_assert_object_property(P, G),
  owl_assert_property(P, Parent, Lbl, Comm, D, R, G, Opts).



%! owl_assert_ontology(+Ontology, ?G) is det.

owl_assert_ontology(Onto, G) :-
  rdf_assert_instance(Onto, owl:'Ontology', G).



%! owl_assert_value_restriction(+P, +Value, ?G, -Restriction) is det.

owl_assert_value_restriction(P, V, G, R) :-
  rdf_create_bnode(R),
  rdf_assert_instance(R, owl:'Restriction', G),
  rdf_assert(R, owl:onProperty, P, G),
  rdf_assert(R, owl:hasValue, V, G).



%! rdf_assert_action(+ActionClass:iri, +Actor:iri, -Action:iri) is det.
%! rdf_assert_action(+ActionClass:iri, +Actor:iri, -Action:iri, +G) is det.

rdf_assert_action(ActionClass, Actor, Action):-
  rdf_assert_action(ActionClass, Actor, Action, default).

rdf_assert_action(ActionClass, Actor, Action, G):-
  rdf_create_iri(vzm, [action], Action),
  rdf_assert(Action, rdf:type, ActionClass, G),
  rdf_assert_now(Action, prov:atTime, G),
  rdf_assert(Actor, sbo:performed, Action, G).



%! rdf_assert_instance(+I, ?Cs, ?G) is det.

rdf_assert_instance(I, C, G) :-
  var(C), !,
  rdf_assert(I, rdf:type, rdfs:'Resource', G).
rdf_assert_instance(I, Cs, G) :-
  is_list(Cs), !,
  maplist([C]>>rdf_assert_instance(I, C, G), Cs).
rdf_assert_instance(I, C, G) :-
  rdf_assert(I, rdf:type, C, G).



%! rdf_assert_now(+S, +P) is det.
%! rdf_assert_now(+S, +P, +D) is det.
%! rdf_assert_now(+S, +P, +D, +G) is det.

rdf_assert_now(S, P) :-
  rdf_assert_now(S, P, xsd:dateTime).

rdf_assert_now(S, P, D) :-
  rdf_assert_now(S, P, D, default).

rdf_assert_now(S, P, D, G) :-
  get_time(Now),
  rdf_assert(S, P, Now^^D, G).



%! rdf_create_iri(+Prefix:atom, -Iri:atom) is det.
%! rdf_create_iri(+Prefix:atom, +SubPaths:list(atom), -Iri:atom) is det.
% Succeeds with a fresh IRI within the RDF namespace denoted by Prefix
% and the given SubPaths.
%
% IRI freshness is guaranteed by the UUID that is used as the path suffix.
%
% @arg Prefix   A registered RDF prefix name.
% @arg SubPaths A list of path names that prefix the UUID.
% @arg Iri      A fresh IRI.

rdf_create_iri(Prefix, Iri) :-
  rdf_create_iri(Prefix, [], Iri).

rdf_create_iri(Prefix, SubPaths0, Iri) :-
  uuid_no_hyphen(Id),
  append(SubPaths0, [Id], SubPaths),
  atomic_list_concat(SubPaths, /, LocalName),
  % Resolve the absolute IRI against the base IRI denoted by the RDF prefix.
  rdf_global_id(Prefix:LocalName, Iri).



%! rdfs_assert_class(+C, ?D, ?Label, ?Comment, ?G) is det.

rdfs_assert_class(C, Parent, Lbl, Comm, G) :-
  rdf_assert(C, rdf:type, rdfs:'Class', G),
  rdfs_assert_class0(C, Parent, Lbl, Comm, G).



%! rdfs_assert_comment(+S, +Comment, ?G) is det.

rdfs_assert_comment(S, Comment, G) :-
  rdf_assert(S, rdfs:comment, Comment, G).



%! rdfs_assert_domain(+P, +C, ?G) is det.

rdfs_assert_domain(P, D, G) :-
  rdf_assert(P, rdfs:domain, D, G).



%! rdfs_assert_isDefinedBy(+S, ?G) is det.
%! rdfs_assert_isDefinedBy(+S, ?Iri, ?G) is det.
% If Iri is uninstantiated, the IRI denoted by the registered RDF prefix
% of Term, if any, is used.

rdfs_assert_isDefinedBy(S, G) :-
  rdfs_assert_isDefinedBy(S, _, G).

rdfs_assert_isDefinedBy(S, Prefix, G) :-
  var(Prefix), !,
  rdf_iri_alias_prefix_local(S, _, Prefix, _),
  rdf_assert(S, rdfs:isDefinedBy, Prefix^^xsd:anyURI, G).
rdfs_assert_isDefinedBy(S, Iri, G) :-
  rdf_assert(S, rdfs:isDefinedBy, Iri^^xsd:anyURI, G).



%! rdfs_assert_label(+S, +Label) is det.
%! rdfs_assert_label(+S, +Label, ?G) is det.
% Assigns an RDFS label to the resource denoted by the given RDF term.
%
% This predicate stores the label as an RDF language-tagged string.
% The default language is `en-US`.

rdfs_assert_label(S, Label) :-
  rdfs_assert_label(S, Label, _).

rdfs_assert_label(S, Label, G) :-
  rdf_assert(S, rdfs:label, Label, G).



%! rdfs_assert_property(+C, +P, +D, ?G) is det.

rdfs_assert_property(C, P, D, G) :-
  rdfs_assert_domain(P, C, G),
  rdfs_assert_range(P, D, G).



%! rdfs_assert_range(+P, +C, ?G) is det.

rdfs_assert_range(P, C, G) :-
  rdf_assert(P, rdfs:range, C, G).



%! rdfs_assert_seeAlso(+S, +Iri, ?G) is det.

rdfs_assert_seeAlso(S, Iri, G) :-
  rdf_assert(S, rdfs:seeAlso, Iri, G).



%! rdfs_assert_subclass(+C, ?G) is det.
%! rdfs_assert_subclass(+C, ?D, ?G) is det.
% If D is uninstantiated it defaults to `rdfs:Resource`.

rdfs_assert_subclass(C, G) :-
  rdfs_assert_subclass(C, _, G).
rdfs_assert_subclass(C, D, G) :-
  % Allow the parent class to be uninstantiated.
  (   var(D)
  ->  rdf_assert(C, rdfs:subClassOf, rdfs:'Resource', G)
  ;   is_list(D)
  ->  forall(member(D0, D), rdf_assert(C, rdfs:subClassOf, D0, G))
  ;   rdf_assert(C, rdfs:subClassOf, D, G)
  ).



%! rdfs_assert_subproperty(+P, ?Q, ?G) is det.
% Creates a new property that is a subproperty of the given parent property.
%
% If Q is uninstantiated it defaults to `rdf:Property`.

rdfs_assert_subproperty(P, Qs, G) :-
  is_list(Qs), !,
  forall(member(Q, Qs), rdfs_assert_subproperty(P, Q, G)).
rdfs_assert_subproperty(P, Q, G) :-
  rdf_defval(rdf:'Property', Q),
  rdf_assert(P, rdfs:subPropertyOf, Q, G).



%! rdfs_retractall_class(+C) is det.
% Removes the given class from the triple store.
%
% This is the same as removing class terms that are closed under identity.
%
% This connects all subclasses of Class to all superclasses of Class.

rdfs_retractall_class(C) :-
  % [1] Remove the links to subclasses.
  %     Connect all subclasses of Class to all superclasses of Class.
  forall(
    (
      rdf_has(SubC, rdfs:subClassOf, C),
      rdf_has(C, rdfs:subClassOf, SuperC)
    ),
    (
      % The transitive link is now a direct one.
      rdfs_assert_subclass(SubC, SuperC, _),
      % Remove the link to a subclass.
      rdf_retractall(SubC, rdfs:subClassOf, C)
    )
  ),

  % [2] Remove the links to superclasses.
  rdf_retractall(C, rdfs:subClassOf, _),

  % [3] Remove other triples in which the class occurs.
  rdf_retractall(C, _, _),
  rdf_retractall(_, C, _),
  rdf_retractall(_, _, C).





% HELPERS %

rdfs_assert_class0(C, Parent, Lbl, Comm, G) :-
  rdfs_assert_subclass(C, Parent, G),
  (var(Lbl) -> true ; rdfs_assert_label(C, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(C, Comm, G)),
  rdfs_assert_isDefinedBy(C, G).


%! owl_assert_property(+P, ?Q, ?Label, ?Comment, ?Domain, ?Range, ?G, +Opts) is det.

owl_assert_property(P, Qs, Lbl, Comm, D, R, G, Opts) :-
  (option(functional(true), Opts) -> owl_assert_functional_property(P, G) ; true),
  (var(Qs) -> true ; is_list(Qs) -> maplist([Q]>>rdfs_assert_subproperty(P, Q, G), Qs) ; rdfs_assert_subproperty(P, Qs, G)),
  (var(Lbl) -> true ; rdfs_assert_label(P, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(P, Comm, G)),
  (var(D) -> true ; rdfs_assert_domain(P, D, G)),
  (var(R) -> true ; rdfs_assert_range(P, R, G)),
  rdfs_assert_isDefinedBy(P, G).


%! uuid_no_hyphen(-Id:atom) is det.

uuid_no_hyphen(Id):-
  uuid(Id0),
  atomic_list_concat(Comps, -, Id0),
  atomic_list_concat(Comps, Id).
