:- module(
  owl_build,
  [
    owl_assert_class/6,               % +M, +C, ?D, ?Lbl, ?Comm, +G
    owl_assert_data_property/9,       % +M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts
    owl_assert_disjointWith/4,        % +M, +C, +D, +G
    owl_assert_equivalent_class/4,    % +M, +C, +D, +G
    owl_assert_functional_property/3, % +M, +P, +G
    owl_assert_identity/4,            % +M, +S, +O, +G
    owl_assert_intersection_of/4,     % +M, +C, +Ds, +G
    owl_assert_named_individual/6,    % +M, +I, +C, ?Lbl, ?Comm, +G
    owl_assert_object_property/9,     % +M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts
    owl_assert_ontology/3,            % +M, +Ontology, +G
    owl_assert_value_restriction/5    % +M, +P, +Val, +G, -Restriction
  ]
).
:- reexport(library(rdfs/rdfs_build)).

/** <module> OWL build API

@author Wouter Beek
@version 2016/06-2017/01
*/

:- use_module(library(default)).
:- use_module(library(option)).

:- rdf_meta
   owl_assert_data_property(+, r, t, ?, ?, r, r, r, +),
   owl_assert_disjointWith(+, o, o, r),
   owl_assert_equivalent_class(+, r, r, r),
   owl_assert_functional_property(+, r, r),
   owl_assert_identity(+, r, r, r),
   owl_assert_intersection_of(+, r, t, r),
   owl_assert_named_individual(+, r, t, ?, ?, r),
   owl_assert_object_property(+, r, t, ?, ?, r, r, r, +),
   owl_assert_ontology(+, r, r),
   owl_assert_value_restriction(+, r, r, r, -).





%! owl_assert_data_property(+M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts) is det.
%
% The following options are supported:
%
%   * functional(+boolean)

owl_assert_data_property(M, P, Q, Lbl, Comm, Dom, Ran, G, Opts) :-
  rdfs_assert_instance(M, P, owl:'DataProperty', G),
  owl_assert_property0(M, P, Q, Lbl, Comm, Dom, Ran, G, Opts).



%! owl_assert_disjointWith(+M, +C, +D, +G) is det.

owl_assert_disjointWith(M, C, D, G) :-
  rdf_assert(M, C, owl:disjointWith, D, G).



%! owl_assert_equivalent_class(+M, +C, +D, +G) is det.

owl_assert_equivalent_class(M, C, D, G) :-
  rdf_assert(M, C, owl:equivalentClass, D, G).



%! owl_assert_functional_property(+M, +P, +G) is det.

owl_assert_functional_property(M, P, G) :-
  rdf_asserty_instance(M, P, owl:'FunctionalProperty', G).



%! owl_assert_identity(+M, +S, +O, +G) is det.

owl_assert_identity(M, S, O, G) :-
  rdf_assert(M, S, owl:sameAs, O, G).



%! owl_assert_intersection_of(+M, ?C, +Ds, +G) is det.

owl_assert_intersection_of(M, C, Ds, G) :-
  defgoal(rdf_create_bnode, C),
  rdf_assert_instance(M, C, owl:'Class', G),
  rdf_assert_list(M, Ds, Ds0, G),
  rdf_assert(M, C, owl:intersectionOf, Ds0, G).



%! owl_assert_named_individual(+M, +I, +C, ?Lbl, ?Comm, +G) is det.

owl_assert_named_individual(M, I, C, Lbl, Comm, G) :-
  rdf_assert_instance(M, I, owl:'NamedIndividual', G),
  (var(C) -> true ; rdf_assert_instance(M, I, C, G)),
  (var(Lbl) -> true ; rdfs_assert_label(M, I, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(M, I, Comm, G)),
  rdfs_assert_isDefinedBy(M, I, G).



%! owl_assert_object_property(+M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts) is det.
%
% The following options are supported:
%
%   * functional(+boolean)

owl_assert_object_property(M, P, Parent, Lbl, Comm, D, R, G, Opts) :-
  rdf_assert_instance(M, P, owl:'ObjectProperty', G),
  owl_assert_property0(M, P, Parent, Lbl, Comm, D, R, G, Opts).



%! owl_assert_ontology(+M, +Ontology, +G) is det.

owl_assert_ontology(M, Onto, G) :-
  rdf_assert_instance(M, Onto, owl:'Ontology', G).



%! owl_assert_value_restriction(+M, +P, +Val, +G, -Restriction) is det.

owl_assert_value_restriction(M, P, V, G, R) :-
  rdf_create_bnode(R),
  rdf_assert_instance(M, R, owl:'Restriction', G),
  rdf_assert(M, R, owl:onProperty, P, G),
  rdf_assert(M, R, owl:hasValue, V, G).





% HELPERS %

%! owl_assert_property0(+M, +P, ?Q, ?Lbl, ?Comm, ?Dom, ?Ran, +G, +Opts) is det.

owl_assert_property0(M, P, Q, Lbl, Comm, Dom, Ran, G, Opts) :-
  (   option(functional(true), Opts)
  ->  owl_assert_functional_property(M, P, G)
  ;   true
  ),
  (var(Q) -> true ; rdfs_assert_subproperty(M, P, Q, G)),
  (var(Lbl) -> true ; rdfs_assert_label(M, P, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(M, P, Comm, G)),
  (var(Dom) -> true ; rdfs_assert_domain(M, P, Dom, G)),
  (var(Ran) -> true ; rdfs_assert_range(M, P, Ran, G)),
  rdfs_assert_isDefinedBy(M, P, G).
