:- module(
  rdfs_ext,
  [
    rdfs_class/1,              % ?C
    rdfs_class/2,              % ?C, ?G
    rdfs_domain/2,             % ?P, ?Dom
    rdfs_domain/3,             % ?P, ?Dom, ?G
    rdfs_has/3,                % ?S, ?P, ?O
    rdfs_has/4,                % ?S, ?P, ?O, ?Q
    rdfs_has/5,                % ?S, ?P, ?O, ?Q, ?G
    rdfs_image/2,              % +S, -Img
    rdfs_instance/2,           % ?I, ?C
    rdfs_instance/3,           % ?I, ?C, ?G
    rdfs_property/1,           % ?Prop
    rdfs_property/2,           % ?Prop, ?G
    rdfs_range/2,              % ?P, ?Ran
    rdfs_range/3,              % ?P, ?Ran, ?G
    rdfs_retractall_class/1    % +C
  ]
).

/** <module> RDFS extensions

@author Wouter Beek
@version 2016/04-2016/06
*/

:- use_module(library(q/qb)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(solution_sequences)).

:- qb_alias(dbo, 'http://dbpedia.org/ontology/').
:- qb_alias(dcmit, 'http://purl.org/dc/dcmitype/').

:- rdf_meta
   rdfs_class(r),
   rdfs_class(r, r),
   rdfs_domain(r, r),
   rdfs_domain(r, r, r),
   rdfs_has(r, r, o),
   rdfs_has(r, r, o, r, r),
   rdfs_image(r, -),
   rdfs_instance(o, r),
   rdfs_instance(o, r, r),
   rdfs_property(r),
   rdfs_property(r, r),
   rdfs_range(r, r),
   rdfs_range(r, r, r),
   rdfs_retractall_class(r).





%! rdfs_class(?C) is semidet.
%! rdfs_class(?C, ?G) is nondet.

rdfs_class(C) :-
  distinct(C, rdfs_class(C, _)).


rdfs_class(C, G) :-
  distinct(C, rdfs_class0(C, G)).


rdfs_class0(C, G) :-
  rdfs_instance(C, rdfs:'Class', G).
rdfs_class0(C, G) :-
  rdfs_has(C, rdfs:subClassOf, _, _, G).
rdfs_class0(C, G) :-
  rdfs_has(_, rdfs:subClassOf, C, _, G).
rdfs_class0(C, G) :-
  rdfs_has(_, rdfs:domain, C, _, G).
rdfs_class0(C, G) :-
  rdfs_has(_, rdfs:range, C, _, G).
rdfs_class0(C, G) :-
  rdfs_has(_, rdf:type, C, _, G).



%! rdfs_domain(?P, ?Dom) is nondet.
%! rdfs_domain(?P, ?Dom, ?G) is nondet.

rdfs_domain(P, Dom) :-
  distinct(P-Dom, rdfs_domain(P, Dom, _)).


rdfs_domain(P, Dom, G) :-
  rdfs_has(P, rdfs:domain, Dom, _, G).



%! rdfs_has(?S, ?P, ?O) is nondet.
%! rdfs_has(?S, ?P, ?O, -Q) is nondet.
%! rdfs_has(?S, ?P, ?O, -Q, ?G) is nondet.

rdfs_has(S, P, O) :-
  rdfs_has(S, P, O, _).


rdfs_has(S, P, O, Q) :-
  rdfs_has(S, P, O, Q, _).


rdfs_has(S, P, O, Q, G) :-
  rdf_has(S, P, O, Q),
  rdf(S, Q, O, G).



%! rdfs_image(+S, -Img) is nondet.

rdfs_image(S, Img) :-
  rdfs_has(S, dbo:thumbnail, Img^^xsd:anyURI).
rdfs_image(S, Img) :-
  rdfs_has(S, foaf:depiction, Img^^xsd:anyURI).
rdfs_image(S, Img) :-
  rdf(S, _, Img),
  rdfs_instance(Img, dcmit:'Image').



%! rdfs_instance(?I, ?C) is nondet.
%! rdfs_instance(?I, ?C, ?G) is nondet.

rdfs_instance(I, D) :-
  distinct(I-D, rdfs_instance(I, D, _)).


rdfs_instance(I, D, G) :-
  distinct(I-D-G, rdfs_instance0(I, D, G)).


rdfs_instance0(I, D, G) :-
  nonvar(D), !,
  rdf_reachable(C, rdfs:subClassOf, D), % @tbd
  rdfs_has(I, rdf:type, C, _, G).
rdfs_instance0(I, D, G) :-
  rdfs_has(I, rdf:type, C, _, G),
  rdf_reachable(C, rdfs:subClassOf, D). % @tbd
rdfs_instance0(Lex^^C, D, G) :-
  rdf(_, _, Lex^^C, G),
  q_subdatatype_of(C, D).
rdfs_instance0(Lex@LTag, rdf:langString, G) :-
  rdf(_, _, Lex@LTag, G).



%! rdfs_property(?Prop) is nondet.
%! rdfs_property(?Prop, ?G) is nondet.

rdfs_property(Prop) :-
  distinct(Prop, rdfs_property(Prop, _)).


rdfs_property(Prop, G) :-
  distinct(Prop-G, rdfs_property0(Prop, G)).


rdfs_property0(Prop, G) :-
  rdf_predicate(Prop, G).
rdfs_property0(Prop, G) :-
  rdfs_instance(Prop, rdf:'Property', G).



%! rdfs_range(?P, ?Ran) is nondet.
%! rdfs_range(?P, ?Ran, ?G) is nondet.

rdfs_range(P, Ran) :-
  rdfs_range(P, Ran, _).


rdfs_range(P, Ran, G) :-
  rdfs_has(P, rdfs:range, Ran, _, G).



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
      rdfs_has(SubC, rdfs:subClassOf, C),
      rdfs_has(C, rdfs:subClassOf, SuperC)
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
  (var(Lbl) -> true ; qb_label(rdf, C, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(C, Comm, G)),
  rdfs_assert_isDefinedBy(C, G).
