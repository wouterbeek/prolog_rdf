:- module(
  rdfs_ext,
  [
    rdfs_assert_class/5,       % +C, ?D, ?Lbl, ?Comment, ?G
    rdfs_assert_comment/2,     % +S, +Comment
    rdfs_assert_comment/3,     % +S, +Comment, ?G
    rdfs_assert_domain/3,      % +P, +C, ?G
    rdfs_assert_isDefinedBy/2, % +S, ?G
    rdfs_assert_isDefinedBy/3, % +S, ?Iri, ?G
    rdfs_assert_label/2,       % +S, +Lbl
    rdfs_assert_label/3,       % +S, +Lbl, ?G
    rdfs_assert_property/4,    % +C, +P, +D, ?G
    rdfs_assert_range/3,       % +P, +C, ?G
    rdfs_assert_seeAlso/3,     % +S, +Uri, +G
    rdfs_assert_subclass/2,    % +C, ?G
    rdfs_assert_subclass/3,    % +C, ?D, ?G
    rdfs_assert_subproperty/3, % +P, +Q, ?G
    rdfs_class/1,              % ?C
    rdfs_domain/2,             % ?P, ?Dom
    rdfs_instance/2,           % ?I, ?C
    rdfs_pref_label/2,         % ?S, -Lit
    rdfs_property/1,           % ?Prop
    rdfs_range/2,              % ?P, ?Ran
    rdfs_retractall_class/1    % +C
  ]
).

/** <module> RDFS extensions

@author Wouter Beek
@version 2016/04-2015/05
*/

:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   rdfs_assert_class(r, t, ?, ?, r),
   rdfs_assert_comment(r, +),
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
   rdfs_class(r),
   rdfs_domain(r, r),
   rdfs_instance(o, r),
   rdfs_pref_label(r, o),
   rdfs_property(r),
   rdfs_range(r, r),
   rdfs_retractall_class(r).





%! rdfs_assert_class(+C, ?D, ?Lbl, ?Comment, ?G) is det.

rdfs_assert_class(C, Parent, Lbl, Comm, G) :-
  rdf_assert(C, rdf:type, rdfs:'Class', G),
  rdfs_assert_class0(C, Parent, Lbl, Comm, G).



%! rdfs_assert_comment(+S, +Comment) is det.
%! rdfs_assert_comment(+S, +Comment, ?G) is det.

rdfs_assert_comment(S, Comment) :-
  rdfs_assert_comment(S, Comment, _).


rdfs_assert_comment(S, Comment, G) :-
  rdf_assert(S, rdfs:comment, Comment, G).



%! rdfs_assert_domain(+P, +C, ?G) is det.

rdfs_assert_domain(P, D, G) :-
  rdf_assert(P, rdfs:domain, D, G).



%! rdfs_assert_isDefinedBy(+S, ?G) is det.
%! rdfs_assert_isDefinedBy(+S, ?Iri, ?G) is det.
%
% If Iri is uninstantiated, the IRI denoted by the registered RDF
% prefix of Term, if any, is used.

rdfs_assert_isDefinedBy(S, G) :-
  rdfs_assert_isDefinedBy(S, _, G).

rdfs_assert_isDefinedBy(S, Prefix, G) :-
  var(Prefix), !,
  rdf_iri_alias_prefix_local(S, _, Prefix, _),
  rdf_assert(S, rdfs:isDefinedBy, Prefix^^xsd:anyURI, G).
rdfs_assert_isDefinedBy(S, Iri, G) :-
  rdf_assert(S, rdfs:isDefinedBy, Iri^^xsd:anyURI, G).



%! rdfs_assert_label(+S, +Lbl) is det.
%! rdfs_assert_label(+S, +Lbl, ?G) is det.
%
% Assigns an RDFS label to the resource denoted by the given RDF term.
%
% This predicate stores the label as an RDF language-tagged string.
% The default language is `en-US`.

rdfs_assert_label(S, Lbl) :-
  rdfs_assert_label(S, Lbl, _).


rdfs_assert_label(S, Lbl, G) :-
  rdf_assert(S, rdfs:label, Lbl, G).



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



%! rdfs_class(+C) is semidet.
%! rdfs_class(-C) is nondet.

rdfs_class(C) :-
  distinct(C, rdfs_class0(C)).

rdfs_class0(C) :-
  rdfs_instance(C, rdfs:'Class').
rdfs_class0(C) :-
  rdfs_subclass_of(C, _).
rdfs_class0(C) :-
  rdfs_subclass_of(_, C).
rdfs_class0(C) :-
  rdf_has(_, rdfs:domain, C).
rdfs_class0(C) :-
  rdf_has(_, rdfs:range, C).
rdfs_class0(C) :-
  rdf_has(_, rdf:type, C).



%! rdfs_domain(?P, ?Dom) is nondet.

rdfs_domain(P, Dom) :-
  rdf_has(P, rdfs:domain, Dom).



%! rdfs_instance(?I, ?C) is nondet.

rdfs_instance(I, D) :-
  nonvar(D), !,
  rdf_reachable(C, rdfs:subClassOf, D),
  rdf_has(I, rdf:type, C).
rdfs_instance(I, D) :-
  rdf_has(I, rdf:type, C),
  rdf_reachable(C, rdfs:subClassOf, D).
rdfs_instance(Lex^^C, D) :-
  rdf(_, _, Lex^^C),
  rdf_subdatatype_of(C, D).
rdfs_instance(Lex@LTag, rdf:langString) :-
  rdf(_, _, Lex@LTag).



%! rdfs_pref_label(?S, -Lit) is nondet.

rdfs_pref_label(S, Lit) :-
  rdf_pref_string(S, rdfs:label, Lit).



%! rdfs_property(-Prop) is nondet.

rdfs_property(Prop) :-
  distinct(Prop, rdfs_property0(Prop)).

rdfs_property0(Prop) :-
  rdf_predicate(Prop).
rdfs_property0(Prop) :-
  rdfs_instance(Prop, rdf:'Property').
rdfs_property0(Prop) :-
  rdfs_subproperty_of(Prop, _).
rdfs_property0(Prop) :-
  rdfs_subproperty_of(_, Prop).



%! rdfs_range(?P, ?Ran) is nondet.

rdfs_range(P, Ran) :-
  rdf_has(P, rdfs:range, Ran).



%! rdfs_retractall_class(+C) is det.
% Removes the given class from the triple store.
%
% This is the same as removing class terms that are closed under
% identity.
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
