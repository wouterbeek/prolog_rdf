:- module(
  rdfs_ext,
  [
    rdfs_class/1,    % ?C
    rdfs_class/2,    % ?C, ?G
    rdfs_has/3,      % ?S, ?P, ?O
    rdfs_has/4,      % ?S, ?P, ?O, ?Q
    rdfs_has/5,      % ?S, ?P, ?O, ?Q, ?G
    rdfs_instance/2, % ?I, ?C
    rdfs_instance/3, % ?I, ?C, ?G
    rdfs_property/1, % ?Prop
    rdfs_property/2, % ?Prop, ?G
    qb_rm_class/3    % ?M, +C, ?G
  ]
).

/** <module> RDFS extensions

@author Wouter Beek
@version 2016/04-2016/07
*/

:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(solution_sequences)).

:- qb_alias(dbo, 'http://dbpedia.org/ontology/').
:- qb_alias(dcmit, 'http://purl.org/dc/dcmitype/').

:- rdf_meta
   rdfs_class(r),
   rdfs_class(r, r),
   rdfs_has(r, r, o),
   rdfs_has(r, r, o, r, r),
   rdfs_instance(o, r),
   rdfs_instance(o, r, r),
   rdfs_property(r),
   rdfs_property(r, r),
   qb_rm_class(?, r, r).





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
  q_predicate(rdf, Prop, G).
rdfs_property0(Prop, G) :-
  rdfs_instance(Prop, rdf:'Property', G).



%! qb_rm_class(+M, +C, +G) is det.
% Removes the given class from the triple store.
%
% This is the same as removing class terms that are closed under
% identity.
%
% This connects all subclasses of Class to all superclasses of Class.

qb_rm_class(M, C, G) :-
  % [1] Remove the links to subclasses.
  %     Connect all subclasses of Class to all superclasses of Class.
  forall(
    (
      q_subclass(M, SubC, C, G),
      q_subclass(M, C, SuperC, G)
    ),
    (
      % The transitive link is now a direct one.
      qb_subclass(M, SubC, SuperC, G),
      % Remove the link to a subclass.
      qb_rm(M, SubC, rdfs:subClassOf, C, G)
    )
  ),

  % [2] Remove the links to superclasses.
  qb_rm(M, C, rdfs:subClassOf, _, G),

  % [3] Remove other triples in which the class occurs.
  qb_rm(M, C, _, _, G),
  qb_rm(M, _, C, _, G),
  qb_rm(M, _, _, C, G).





% HELPERS %

rdfs_assert_class0(M, C, Parent, Lbl, Comm, G) :-
  qb_subclass(M, C, Parent, G),
  (var(Lbl) -> true ; qb_label(M, C, Lbl, G)),
  (var(Comm) -> true ; qb_comment(M, C, Comm, G)),
  qb_isDefinedBy(M, C, G).
