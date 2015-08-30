:- module(
  id_store,
  [
    id_term/2, % +IdSet:uid
               % -Term:rdf_term
    id_terms/2, % +IdSet:uid
                % -Terms:ordset(rdf_term)
    rdf_assert3/3, % +Subject:rdf_term
                   % +Predicate:iri
                   % +Object:rdf_term
    rdf3/3, % ?Subject:rdf_term
            % ?Predicate:iri
            % ?Object:rdf_term
    term_id/2 % +Term:rdf_term
              % -IdSet:uid
  ]
).

/** <module> Identity store

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uuid_ext)).

:- rdf_meta(rdf_assert3(o,r,o)).
:- rdf_meta(rdf3(o,r,o)).

%! id_terms0(?IdSet:uid, ?Terms:ordset(rdf_term)) is nondet.

:- dynamic(id_terms0/2).

%! term_id0(?Term:rdf_term, IdSet:uid) is nondet.

:- dynamic(term_id0/2).

:- predicate_options(print_id/2, 2, [
     pass_to(print_term/2, 2)
   ]).





%! id_term(+IdSet:uid, -Term:rdf_term) is multi.

id_term(Id, T):-
  id_terms0(Id, Ts),
  member(T, Ts).



%! id_terms(+IdSet:uid, -Terms:ordset(rdf_term)) is multi.

id_terms(Id, Ts):-
  id_terms0(Id, Ts).



%! rdf_assert3(+Subject:rdf_term, +Predicate:iri, +Object:rdf_term) is det.

rdf_assert3(S, P, O):-
  rdf_global_id(owl:sameAs, P), !,
  store_id(S, O).
rdf_assert3(S, P, O):-
  maplist(term_id, [S,P,O], [S0,P0,O0]),
  rdf_assert(S0, P0, O0).



%! rdf3(?Subject:rdf_term, ?Predicate:iri, ?Object:rdf_term) is nondet.

rdf3(S, P, O):-
  term_id(S, SId),
  term_id(P, PId),
  term_id(O, OId),
  rdf(SId, PId, OId),
  (var(S) -> id_term(SId, S) ; true),
  (var(P) -> id_term(PId, P) ; true),
  (var(O) -> id_term(OId, O) ; true).



%! store_id(+X:rdf_term, +Y:rdf_term) is det.

store_id(X, Y):-
  (   term_id(X, XId)
  ->  (   term_id(Y, YId)
      ->  (   % X and Y already belong to the same identity set.
              XId == YId
          ->  true
          ;   % Merge the identity sets of X and Y.
              id_terms0(XId, Xs),
              id_terms0(YId, Ys),
              ord_union(Xs, Ys, Zs),
              uuid_no_hyphen(ZId),
              with_mutex(store_id, (
                retract(term_id0(X, XId)),
                retract(id_terms0(XId, Xs)),
                retract(term_id0(Y, YId)),
                retract(id_terms0(YId, Ys)),
                assert(term_id0(X, ZId)),
                assert(term_id0(Y, ZId)),
                assert(id_terms0(ZId, Zs))
             ))
          )
      ;   % Add Y to the identity set of X.
          id_terms0(XId, Xs1),
          ord_add_element(Xs1, Y, Xs2),
          with_mutex(store_id, (
            retract(id_terms0(XId, Xs1)),
            assert(id_terms0(XId, Xs2)),
            assert(term_id0(Y, XId))
          ))
      )
  ;   term_id(Y, YId)
  ->  % Add X to the identity set of Y.
      id_terms0(YId, Ys1),
      ord_add_element(Ys1, X, Ys2),
      with_mutex(store_id, (
        retract(id_terms0(YId, Ys1)),
        assert(id_terms0(YId, Ys2)),
        assert(term_id0(X, YId))
      ))
  ;   uuid_no_hyphen(XId),
      uuid_no_hyphen(YId),
      with_mutex(store_id, (
        store_term0(X, XId),
        store_term0(Y, YId)
      ))
  ).



%! term_id(+Term:rdf_term, -IdentitySet:uri) is det.

term_id(T, T):-
  var(T), !.
term_id(T, TId):-
  term_id0(T, TId), !.
term_id(T, TId):-
  uuid_no_hyphen(TId),
  with_mutex(store_id, store_term0(T, TId)).





% HELPERS %

%! store_term0(+Term:rdf_term, +TermId:uid) is det.

store_term0(T, TId):-
  assert(term_id0(T, TId)),
  assert(id_terms0(TId, [T])).
