:- module(
  hdt_term,
  [
    hdt_bnode/1,     % ?B
    hdt_bnode/2,     % ?B, ?G
    hdt_datatype/1,  % ?D
    hdt_datatype/2,  % ?D, ?G
    hdt_iri/1,       % ?Iri
    hdt_iri/2,       % ?Iri, ?G
    hdt_literal/1,   % ?Lit
    hdt_literal/2,   % ?Lit, ?G
    hdt_lts/1,       % ?Lit
    hdt_lts/2,       % ?Lit, ?G
    hdt_name/1,      % ?Name, ?G
    hdt_name/2,      % ?Name, ?G
    hdt_node/1,      % ?Node, ?G
    hdt_node/2,      % ?Node, ?G
    hdt_object/1,    % ?O, ?G
    hdt_object/2,    % ?O, ?G
    hdt_predicate/1, % ?P, ?G
    hdt_predicate/2, % ?P, ?G
    hdt_subject/1,   % ?S, ?G
    hdt_subject/2,   % ?S, ?G
    hdt_term/1,      % ?Term, ?G
    hdt_term/2       % ?Term, ?G
  ]
).

/** <module> HDT terms

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(hdt), []).
:- use_module(library(hdt/hdt__io)).
:- use_module(library(hdt/hdt_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   hdt_bnode(?, r),
   hdt_datatype(r),
   hdt_datatype(r, r),
   hdt_iri(r),
   hdt_iri(r, r),
   hdt_literal(o),
   hdt_literal(o, r),
   hdt_lts(o),
   hdt_lts(o, r),
   hdt_name(o),
   hdt_name(o, r),
   hdt_node(o),
   hdt_node(o, r),
   hdt_object(o),
   hdt_object(o, r),
   hdt_predicate(r),
   hdt_predicate(r, r),
   hdt_subject(r),
   hdt_subject(r, r),
   hdt_term(o),
   hdt_term(o, r).





%! hdt_bnode(?B) is nondet.
%! hdt_bnode(?B, ?G) is nondet.

hdt_bnode(B) :-
  hdt_bnode(B, _).


hdt_bnode(B, G) :-
  (var(B) -> true ; q_is_bnode(B)),
  distinct(B-G, (
    hdt(S, _, O, G),
    (S = B ; O = B)
  )).



%! hdt_datatype(?D) is nondet.
%! hdt_datatype(?D, ?G) is nondet.

hdt_datatype(D) :-
  distinct(D, hdt_datatype(D, _)).


hdt_datatype(D, G) :-
  distinct(D-G, (
    hdt_literal(Lit, G),
    q_literal_datatype(Lit, D)
  )).



%! hdt_iri(?Iri) is nondet.
%! hdt_iri(?Iri, ?G) is nondet.

hdt_iri(Iri) :-
  distinct(Iri, hdt_iri(Iri, _)).


hdt_iri(Iri, G) :-
  hdt_name(Iri, G),
  q_is_iri(Iri).



%! hdt_literal(?Lit) is nondet.
%! hdt_literal(?Lit, ?G) is nondet.

hdt_literal(Lit) :-
  distinct(Lit, hdt_literal(Lit, _)).


hdt_literal(Lit, G) :-
  hdt_object(Lit, G),
  q_is_literal(Lit).



%! hdt_lts(?Lit) is nondet.
%! hdt_lts(?Lit, ?G) is nondet.

hdt_lts(Lit) :-
  distinct(Lit, hdt_lts(Lit, _)).


hdt_lts(Lit, G) :-
  hdt_literal(Lit, G),
  q_is_lts(Lit).



%! hdt_name(?Name) is nondet.
%! hdt_name(?Name, ?G) is nondet.

hdt_name(Name) :-
  distinct(Name, hdt_name(Name, _)).


hdt_name(Name, G) :-
  distinct(Name-G, (
    hdt__graph(G, Hdt),
    (  hdt:hdt_subject(Hdt, Name)
    ;  hdt:hdt_predicate(Hdt, Name)
    ;  hdt:hdt_object(Hdt, Name)
    ;  hdt:hdt_shared(Hdt, Name)
    )
  )).



%! hdt_node(?Node) is nondet.
%! hdt_node(?Node, ?G) is nondet.

hdt_node(Node) :-
  distinct(Node, hdt_node(Node, _)).


hdt_node(S, G) :-
  hdt_subject(S, G).
hdt_node(O, G) :-
  hdt_object(O, G),
  % Make sure there are no duplicates.
  \+ hdt_subject(O, G).



%! hdt_object(?O) is nondet.
%! hdt_object(?O, ?G) is nondet.

hdt_object(O) :-
  distinct(O, hdt_object(O, _)).


hdt_object(O, G) :-
  distinct(O-G, hdt(_, _, O, G)).



%! hdt_predicate(?P) is nondet.
%! hdt_predicate(?P, ?G) is nondet.

hdt_predicate(P) :-
  distinct(P, hdt_predicate(P, _)).


hdt_predicate(P, G) :-
  (var(P) -> true ; q_is_predicate(P)),
  distinct(P-G, hdt(_, P, _, G)).



%! hdt_subject(?S) is nondet.
%! hdt_subject(?S, ?G) is nondet.

hdt_subject(S) :-
  distinct(S, hdt_subject(S, _)).


hdt_subject(S, G) :-
  (var(S) -> true ; q_is_subject(S)),
  distinct(S-G, hdt(S, _, _, G)).



%! hdt_term(?Term) is nondet.
%! hdt_term(?Term, ?G) is nondet.

hdt_term(Term) :-
  distinct(Term, hdt_term(Term, _)).


hdt_term(Name, G) :-
  hdt_name(Name, G).
hdt_term(B, G) :-
  hdt_bnode(B, G).
