:- module(
  rdf_term,
  [
    rdf_bnode/2,     % ?B, ?G
    rdf_datatype/1,  % ?D
    rdf_datatype/2,  % ?D, ?G
    rdf_iri/2,       % ?Iri, ?G
    rdf_literal/2,   % ?Lit, ?G
    rdf_lts/1,       % ?Lit
    rdf_lts/2,       % ?Lit, ?G
    rdf_name/2,      % ?Name, ?G
    rdf_node/2,      % ?Node, ?G
    rdf_object/2,    % ?O, ?G
    rdf_predicate/2, % ?P, ?G
    rdf_subject/2,   % ?S, ?G
    rdf_term/2       % ?Term, ?G
  ]
).

/** <module> RDF term API

In addition to what `library(semweb/rdf11)` already provides.

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_bnode(?, r),
   rdf_datatype(r),
   rdf_datatype(r, r),
   rdf_iri(r, r),
   rdf_literal(o, r),
   rdf_lts(o),
   rdf_lts(o, r),
   rdf_name(o, r),
   rdf_node(o, r),
   rdf_subject(r, r),
   rdf_object(o, r),
   rdf_predicate(r, r),
   rdf_term(o, r).





%! rdf_bnode(?B, ?G) is nondet.

rdf_bnode(B, G) :-
  rdf_bnode(B),
  distinct(G, rdf_term(B, G)).



%! rdf_datatype(?D) is nondet.
%! rdf_datatype(?D, ?G) is nondet.

rdf_datatype(D) :-
  distinct(D, (
    rdf_literal(Lit),
    q_literal_datatype(Lit, D)
  )).


rdf_datatype(D, G) :-
  distinct(D-G, (
    rdf_literal(Lit, G),
    q_literal_datatype(Lit, D)
  )).



%! rdf_iri(?Iri, ?G) is nondet.

rdf_iri(Iri, G) :-
  rdf_iri(Iri),
  distinct(G, rdf_term(Iri, G)).



%! rdf_literal(?Lit, ?G) is nondet.

rdf_literal(Lit, G) :-
  rdf_literal(Lit),
  distinct(G, rdf(_, _, Lit, G)).



%! rdf_lts(?Lit) is nondet.
%
% The **language-tagged string**s are the cartesian product of the
% Unicode strings in Normal Form C with the set of BCP 47 language
% tags.

rdf_lts(Lit) :-
  rdf_literal(Lit),
  q_is_lts(Lit).



%! rdf_lts(?Lit, ?G) is nondet.

rdf_lts(Lit, G) :-
  rdf_literal(Lit, G),
  q_is_lts(Lit).



%! rdf_name(?Name, ?G) is nondet.

rdf_name(Name, G) :-
  rdf_term(Name, G),
  \+ rdf_is_bnode(Name).



%! rdf_node(?Node, ?G) is nondet.

rdf_node(S, G) :-
  rdf_subject(S, G).
rdf_node(O, G) :-
  rdf_object(O, G),
  % Make sure there are no duplicates.
  \+ rdf_subject(O, G).



%! rdf_object(?O, ?G) is nondet.

rdf_object(O, G) :-
  % [O] In memory we can pre-enumerate (pre-check is idle).
  (var(O) -> rdf_object(O) ; true),
  distinct(G, rdf(_, _, O, G)).



%! rdf_predicate(?P, ?G) is nondet.

rdf_predicate(P, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (var(P) -> rdf_predicate(P) ; rdf_is_iri(P)),
  distinct(G, rdf(_, P, _, G)).



%! rdf_subject(?S, ?G) is nondet.

rdf_subject(S, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (var(S) -> rdf_subject(S) ; rdf_is_subject(S)),
  (var(G) -> distinct(G, rdf(S, _, _, G)) ; once(rdf(S, _, _, G))).



%! rdf_term(?Term, ?G) is nondet.

rdf_term(P, G) :-
  rdf_predicate(P, G).
rdf_term(Node, G) :-
  rdf_node(Node, G),
  % Ensure there are no duplicates.
  \+ rdf_predicate(Node, G).
