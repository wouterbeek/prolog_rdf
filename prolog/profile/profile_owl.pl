:- module(profile_owl, []).

/** <module> OWL

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(list_ext)).
:- use_module(library(owl/id_store)).
:- use_module(library(semweb/rdf_db)).

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
  rdf_db:rdf_assert(S, P, O).
user:rdf_assert(rdf(S,P,O,G)):-
  rdf_db:rdf_assert(S, P, O, G).


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
  store_id(S, O).
user:rdf_assert(S, P, O, G):-
  maplist(term_id, [S,P,O], [SId,PId,OId]),
  (   var(G)
  ->  rdf_db:rdf_assert(SId, PId, OId)
  ;   rdf_db:rdf_assert(SId, PId, OId, G)
  ).



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
