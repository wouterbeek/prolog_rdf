:- module(profile_owl, []).

/** <module> OWL

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(list_ext)).
:- use_module(library(owl/id_store)).
:- use_mdoule(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- rdf_meta(user:rdf(o,r,o)).
:- rdf_meta(user:rdf(o,r,o,?)).
:- rdf_meta(user:rdf_assert(t)).
:- rdf_meta(user:rdf_assert(o,r,o)).
:- rdf_meta(user:rdf_assert(o,r,o,?)).
:- rdf_meta(user:rdf_retractall(o,r,o)).
:- rdf_meta(user:rdf_retractall(o,r,o,?)).





%! user:rdf(?Subject:rdf_term, ?Predicate:iri, ?Object:rdf_term) is nondet.
% Variant of rdf_db:rdf/3 that allows literals in the subject position.

user:rdf(S, P, O):-
  user:rdf(S, P, O, '*').


%! user:rdf(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Variant of rdf_db:rdf/4 that allows literals in the subject position.

user:rdf(S, P, O, _):-
  rdf_equal(owl:sameAs, P),
  (   nonvar(S)
  ->  term_term(S, O)
  ;   nonvar(O)
  ->  term_term(O, S)
  ;   % Enumerate identical terms.
      id_terms(_, Ts),
      member(S, O, Ts)
  ).
user:rdf(S, P, O, G):-
  (nonvar(S) -> (S = id(SId) -> true ; term_id(S, SId)) ; true),
  (nonvar(P) -> term_id(P, PId) ; true),
  (nonvar(O) -> term_id(O, OId) ; true),
  (G == '*' -> rdf_db:rdf(SId, PId, OId) ; rdf_db:rdf(SId, PId, OId, G)),
  (ground(S) -> true ; id_term(SId, S)),
  id_term(PId, P),
  id_term(OId, O).



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

user:rdf_assert(S, P, O, _):-
  rdf_equal(P, owl:sameAs), !,
  maplist(rdf_normalize, [S,O], [SNorm,ONorm]),
  maplist(term_id, [SNorm,ONorm], [SId,OId]),
  store_id(SId, OId).
user:rdf_assert(S, P, O, G):-
  var(G), !,
  maplist(rdf_normalize, [S,P,O], [SNorm,PNorm,ONorm]),
  maplist(term_id, [SNorm,PNorm,ONorm], [SId,PId,OId]),
  rdf_db:rdf_assert(SId, PId, OId).
user:rdf_assert(S, P, O, G):-
  maplist(rdf_normalize, [S,P,O,G], [SNorm,PNorm,ONorm,GNorm]),
  maplist(term_id, [SNorm,PNorm,ONorm,GNorm], [SId,PId,OId,GId]),
  rdf_db:rdf_assert(SId, PId, OId, GId).



%! user:rdf_retractall(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term
%! ) is det.

user:rdf_retractall(S, P, O):-
  maplist(term_id, [S,P,O], [SId,PId,OId]),
  rdf_db:rdf_retractall(SId, PId, OId),
  maplist(remove_id, [SId,PId,OId]).


%! user:rdf_retractall(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.

user:rdf_retractall(S, P, O, G):-
  maplist(term_id, [S,P,O], [SId,PId,OId]),
  rdf_db:rdf_retractall(SId, PId, OId, G),
  maplist(remove_id, [SId,PId,OId]).



user:rdf_subject(S):-
  nonvar(S), !,
  term_id(S, SId),
  rdf_db:rdf_subject(SId).
user:rdf_subject(S):-
  rdf_db:rdf_subject(SId),
  id_term(SId, S).





% HELPERS %

rdf_normalize(X, Y):-
  rdf_is_iri(X), !,
  iri_normalized(X, Y).
rdf_normalize(X, X).
