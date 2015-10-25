:- module(
  profile_gen,
  [
    bnode_literal/2 % ?Subject:bnode
                    % ?Literal:literal
  ]
).

/** <module> Generalized RDF

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_mdoule(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- rdf_meta(bnode_literal(?,o)).
:- rdf_meta(user:rdf(o,r,o)).
:- rdf_meta(user:rdf(o,r,o,?)).
:- rdf_meta(user:rdf_assert(t)).
:- rdf_meta(user:rdf_assert(o,r,o)).
:- rdf_meta(user:rdf_assert(o,r,o,?)).
:- rdf_meta(user:rdf_retractall(o,r,o)).
:- rdf_meta(user:rdf_retractall(o,r,o,?)).

%! bnode_literal(?Subject:bnode, ?Literal:compound) is nondet.

:- dynamic(bnode_literal/2).





%! user:rdf(?Subject:rdf_term, ?Predicate:iri, ?Object:rdf_term) is nondet.
% Variant of user:rdf/3 that allows literals in the subject position.

% Literals may be represented by blank nodes.
user:rdf(Lit, P, O):-
  rdf_is_literal(Lit),
  bnode_literal(BNode, Lit), !,
  rdf_db:rdf(BNode, P, O).
% Variable subject terms may be blank nodes
% that need to be related to literals.
user:rdf(S, P, O):-
  var(S), !,
  rdf_db:rdf(S0, P, O),
  (   rdf_is_bnode(S0),
      bnode_literal(S0, S)
  ->  true
  ;   S = S0
  ).
user:rdf(S, P, O):-
  rdf_db:rdf(S, P, O).


%! user:rdf(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Variant of user:rdf/4 that allows literals in the subject position.

user:rdf(S0, P, O, G):-
  rdf_is_literal(S0),
  bnode_literal(S, S0), !,
  rdf_db:rdf(S, P, O, G).
user:rdf(S, P, O, G):-
  var(S), !,
  rdf_db:rdf(S0, P, O, G),
  (   rdf_is_bnode(S0),
      bnode_literal(S0, S)
  ->  true
  ;   S = S0
  ).
user:rdf(S, P, O, G):-
  rdf_db:rdf(S, P, O, G).



%! user:rdf_assert(+Statement:compound) is det.
% Wrapper around user:rdf_assert/[3,4] for terms `rdf/[3,4]`.

user:rdf_assert(rdf(S,P,O)):- !,
  user:rdf_assert(S, P, O).
user:rdf_assert(rdf(S,P,O,G)):-
  user:rdf_assert(S, P, O, G).


%! user:rdf_assert(+Subject:rdf_term, +Predicate:iri, +Object:rdf_term) is det.
% Wrapper around user:rdf_assert/4 with variable graph.

user:rdf_assert(S, P, O):-
  user:rdf_assert(S, P, O, _).


%! user:rdf_assert(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.
% Alternative of rdf_db:rdf_assert/4 that allows literals to appear
% in the subject positions.

user:rdf_assert(Lit, P, O, G):-
  rdf_is_literal(Lit), !,
  assert_bnode_literal(BNode, Lit),
  user:rdf_assert(BNode, P, O, G).
user:rdf_assert(S, P, O, G):-
  var(G), !,
  maplist(rdf_normalize, [S,P,O], [SNorm,PNorm,ONorm]),
  rdf_db:rdf_assert(SNorm, PNorm, ONorm).
user:rdf_assert(S, P, O, G):-
  maplist(rdf_normalize, [S,P,O,G], [SNorm,PNorm,ONorm,GNorm]),
  rdf_db:rdf_assert(SNorm, PNorm, ONorm, GNorm).



user:rdf_iri(T):-
  nonvar(T), !,
  rdf_is_iri(T),
  (rdf_current_predicate(T) ; rdf_resource(T)), !.
user:rdf_iri(P):-
  rdf_current_predicate(P).
user:rdf_iri(N):-
  rdf_resource(N),
  \+ rdf_is_bnode(N),
  \+ rdf_current_predicate(N).



%! user:rdf_retractall(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term
%! ) is det.

user:rdf_retractall(S0, P, O):-
  rdf_is_literal(S0), !,
  bnode_literal(S, S0),
  rdf_db:rdf_retractall(S, P, O).
user:rdf_retractall(S, P, O):-
  rdf_db:rdf_retractall(S, P, O).


%! user:rdf_retractall(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.

user:rdf_retractall(S0, P, O, G):-
  rdf_is_literal(S0), !,
  bnode_literal(S, S0),
  rdf_db:rdf_retractall(S, P, O, G).
user:rdf_retractall(S, P, O, G):-
  rdf_db:rdf_retractall(S, P, O, G).



user:rdf_subject(S):-
  nonvar(S), !,
  (   rdf_db:rdf_subject(S),
      \+ bnode_literal(S, _)
  ;   bnode_literal(S0, S),
      rdf_db:rdf_subject(S0)
  ), !.
user:rdf_subject(S):-
  rdf_db:rdf_subject(S0),
  (   rdf_is_bnode(S0),
      bnode_literal(S0, S)
  ->  true
  ;   S = S0
  ).





% HELPERS %

%! assert_bnode_literal(-Subject:bnode, +Literal:literal) is det.

assert_bnode_literal(S, Lit):-
  bnode_literal(S, Lit), !.
assert_bnode_literal(S, Lit):-
  rdf_bnode(S),
  assert(bnode_literal(S, Lit)).



rdf_normalize(X, Y):-
  rdf_is_iri(X), !,
  iri_normalized(X, Y).
rdf_normalize(X, X).
