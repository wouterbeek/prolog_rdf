:- module(
  hdt_term,
  [
    hdt_bnode/2,     % ?B, ?G
    hdt_datatype/1,  % ?D
    hdt_datatype/2,  % ?D, ?G
    hdt_iri/2,       % ?Iri, ?G
    hdt_literal/2,   % ?Lit, ?G
    hdt_lts/1,       % ?Lit
    hdt_lts/2,       % ?Lit, ?G
    hdt_name/2,      % ?Name, ?G
    hdt_node/2,      % ?Node, ?G
    hdt_object/2,    % ?O, ?G
    hdt_predicate/2, % ?P, ?G
    hdt_subject/2,   % ?S, ?G
    hdt_term/2       % ?Term, ?G
  ]
).

/** <module> HDT terms

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(z/z_term)).

:- rdf_meta
   hdt_bnode(?, r),
   hdt_datatype(r),
   hdt_datatype(r, r),
   hdt_iri(r, r),
   hdt_literal(o, r),
   hdt_literal(o, r, ?, ?),
   hdt_lts(o),
   hdt_lts(o, r).





%! hdt_bnode(?B, ?G) is nondet.

hdt_bnode(B, G) :-
  z_bnode0(disk, B, G).



%! hdt_datatype(?D) is nondet.
%! hdt_datatype(?D, ?G) is nondet.

hdt_datatype(D) :-
  z_datatype(disk, D).


hdt_datatype(D, G) :-
  z_datatype(disk, D, G).



%! hdt_iri(?Iri, ?G) is nondet.

hdt_iri(Iri, G) :-
  z_iri(disk, Iri, G).



%! hdt_lts(?Lit) is nondet.
%! hdt_lts(?Lit, ?G) is nondet.

hdt_lts(Lit) :-
  z_lts(disk, Lit).


hdt_lts(Lit, G) :-
  z_lts(disk, Lit, G).



%! hdt_literal(?Lit, ?G) is nondet.

hdt_literal(Lit, G) :-
  z_literal(disk, Lit, G).



%! hdt_name(?Name, ?G) is nondet.

hdt_name(Name, G) :-
  z_name(disk, Name, G).



%! hdt_node(?Node, ?G) is nondet.

hdt_node(Node, G) :-
  z_node(disk, Node, G).



%! hdt_object(?O, ?G) is nondet.

hdt_object(O, G) :-
  z_object(disk, O, G).



%! hdt_predicate(?P, ?G) is nondet.

hdt_predicate(P, G) :-
  z_predicate(disk, P, G).



%! hdt_subject(?S, ?G) is nondet.

hdt_subject(S, G) :-
  z_subject(disk, S, G).



%! hdt_term(?Term, ?G) is nondet.

hdt_term(P, G) :-
  z_term(disk, P, G).
