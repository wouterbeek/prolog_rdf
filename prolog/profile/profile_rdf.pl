:- module(profile_rdf, []).

/** <module> RDF profile

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- rdf_meta(user:rdf(r,r,o)).
:- rdf_meta(user:rdf(r,r,o,?)).
:- rdf_meta(user:rdf_assert(t)).
:- rdf_meta(user:rdf_assert(o,r,o)).
:- rdf_meta(user:rdf_assert(o,r,o,?)).





%! user:rdf(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term
%! ) is nondet.

user:rdf(S, P, O):-
  rdf_db:rdf(S, P, O).
  

%! user:rdf(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.

user:rdf(S, P, O, G):-
  rdf_db:rdf(S, P, O, G).
  


%! user:rdf_assert(+Statement:compound) is det.
% Wrapper around user:rdf_assert/[3,4] for terms `rdf/[3,4]`.

user:rdf_assert(rdf(S,P,O)):- !,
  user:rdf_assert(S, P, O).
user:rdf_assert(rdf(S,P,O,G)):-
  user:rdf_assert(S, P, O, G).


%! user:rdf_assert(+Subject:rdf_term, +Predicate:iri, +Object:rdf_term) is det.
% Wrapper around user:rdf_assert/4.

user:rdf_assert(S, P, O):-
  user:rdf_assert(S, P, O, _).


%! user:rdf_assert(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.
% Alternative of rdf_db:rdf_assert/4 that allows Graphs to be uninstantiated
% and that allows literals to appear in the subject positions.

user:rdf_assert(S, P, O, G):-
  var(G), !,
  maplist(rdf_normalize, [S,P,O], [SNorm,PNorm,ONorm]),
  rdf_db:rdf_assert(SNorm, PNorm, ONorm).
user:rdf_assert(S, P, O, G):-
  maplist(rdf_normalize, [S,P,O,G], [SNorm,PNorm,ONorm,GNorm]),
  rdf_db:rdf_assert(SNorm, PNorm, ONorm, GNorm).



user:rdf_retractall(S, P, O, G):-
  maplist(rdf_normalize, [S,P,O,G], [SNorm,PNorm,ONorm,GNorm]),
  rdf_db:rdf_retractall(SNorm, PNorm, ONorm, GNorm).



%! rdf_subject(?Subject:or([bnode,iri])) is nondet.

user:rdf_subject(S):-
  rdf_db:rdf_subject(S).





% HELPERS %

rdf_normalize(X, Y):-
  rdf_is_iri(X), !,
  iri_normalized(X, Y).
rdf_normalize(X, X).
