:- module(
  owl_ext,
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
    owl_assert_value_restriction/4    % +P, +Value, ?G, -Restriction
  ]
).

/** <module> OWL extensions

Predicates for asseritng RDFS statements in an easy way.

@author Wouter Beek
@version 2015/07-2015/09, 2015/12-2016/01, 2016/04-2016/05
*/

:- use_module(library(apply)).
:- use_module(library(default)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

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
   owl_assert_value_restriction(r, r, r, -).





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





% HELPERS %

%! owl_assert_property(+P, ?Q, ?Lbl, ?Comment, ?Domain, ?Range, ?G, +Opts) is det.

owl_assert_property(P, Qs, Lbl, Comm, Dom, Ran, G, Opts) :-
  (option(functional(true), Opts) -> owl_assert_functional_property(P, G) ; true),
  (   var(Qs)
  ->  true
  ;   is_list(Qs)
  ->  maplist({P,G}/[Q]>>rdfs_assert_subproperty(P, Q, G), Qs)
  ;   rdfs_assert_subproperty(P, Qs, G)
  ),
  (var(Lbl) -> true ; rdfs_assert_label(P, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(P, Comm, G)),
  (var(Dom) -> true ; rdfs_assert_domain(P, Dom, G)),
  (var(Ran) -> true ; rdfs_assert_range(P, Ran, G)),
  rdfs_assert_isDefinedBy(P, G).
