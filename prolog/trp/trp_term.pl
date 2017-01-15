:- module(
  trp_term,
  [
    trp_bnode/2,     % ?Bnode, ?G
    trp_datatype/1,  % ?D
    trp_datatype/2,  % ?D,     ?G
    trp_iri/2,       % ?Iri,   ?G
    trp_literal/2,   % ?Lit,   ?G
    trp_lts/1,       % ?Lit
    trp_lts/2,       % ?Lit,   ?G
    trp_name/2,      % ?Name,  ?G
    trp_node/2,      % ?Node,  ?G
    trp_object/2,    % ?O,     ?G
    trp_predicate/2, % ?P,     ?G
    trp_subject/2,   % ?S,     ?G
    trp_term/2       % ?Term,  ?G
  ]
).

/** <module> Memory-based term enumerators

Extend the term enumeration support of module ‘semweb/rdf11’.

@author Wouter Beek
@version 2016/06, 2017/01
*/

:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11), []).
:- use_module(library(solution_sequences)).

:- rdf_meta
   trp_bnode(?, r),
   trp_datatype(r),
   trp_datatype(r, r),
   trp_iri(r, r),
   trp_literal(o, r),
   trp_lts(o),
   trp_lts(o, r),
   trp_name(o, r),
   trp_node(o, r),
   trp_subject(r, r),
   trp_object(o, r),
   trp_predicate(r, r),
   trp_term(o, r).





%! trp_bnode(?BNode, ?G) is nondet.

trp_bnode(BNode, G) :-
  rdf11:rdf_bnode(BNode),
  distinct(G, trp_term(BNode, G)).



%! trp_datatype(?D) is nondet.
%! trp_datatype(?D, ?G) is nondet.

trp_datatype(D) :-
  distinct(D, (
    rdf11:rdf_literal(Lit),
    rdf_literal_datatype(Lit, D)
  )).


trp_datatype(D, G) :-
  distinct(D-G, (
    rdf11:rdf_literal(Lit, G),
    rdf_literal_datatype(Lit, D)
  )).



%! trp_iri(?Iri, ?G) is nondet.

trp_iri(Iri, G) :-
  rdf11:rdf_iri(Iri),
  distinct(G, trp_term(Iri, G)).



%! trp_literal(?Lit, ?G) is nondet.

trp_literal(Lit, G) :-
  rdf11:rdf_literal(Lit),
  distinct(G, rdf11:rdf(_, _, Lit, G)).



%! trp_lts(?Lit) is nondet.
%
% The **language-tagged string**s are the cartesian product of the
% Unicode strings in Normal Form C with the set of BCP 47 language
% tags.

trp_lts(Lit) :-
  rdf11:rdf_literal(Lit),
  rdf_is_lts(Lit).



%! trp_lts(?Lit, ?G) is nondet.

trp_lts(Lit, G) :-
  trp_literal(Lit, G),
  rdf_is_lts(Lit).



%! trp_name(?Name, ?G) is nondet.

trp_name(Name, G) :-
  trp_term(Name, G),
  \+ rdf_is_bnode(Name).



%! trp_node(?Node, ?G) is nondet.

trp_node(S, G) :-
  trp_subject(S, G).
trp_node(O, G) :-
  trp_object(O, G),
  % Make sure there are no duplicates.
  \+ trp_subject(O, G).



%! trp_object(?O, ?G) is nondet.

trp_object(O, G) :-
  % [O] In memory we can pre-enumerate (pre-check is idle).
  (var(O) -> rdf11:rdf_object(O) ; true),
  distinct(G, rdf11:rdf(_, _, O, G)).



%! trp_predicate(?P, ?G) is nondet.

trp_predicate(P, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (var(P) -> rdf11:rdf_predicate(P) ; rdf_is_iri(P)),
  distinct(G, rdf11:rdf(_, P, _, G)).



%! trp_subject(?S, ?G) is nondet.

trp_subject(S, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (var(S) -> rdf_subject(S) ; rdf_is_subject(S)),
  (var(G) -> distinct(G, rdf11:rdf(S, _, _, G)) ; once(rdf11:rdf(S, _, _, G))).



%! trp_term(?Term, ?G) is nondet.

trp_term(P, G) :-
  trp_predicate(P, G).
trp_term(Node, G) :-
  trp_node(Node, G),
  % Ensure there are no duplicates.
  \+ trp_predicate(Node, G).
