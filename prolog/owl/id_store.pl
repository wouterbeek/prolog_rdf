:- module(
  id_store,
  [
    id_term/2, % +IdSet:uid
               % -Term:rdf_term
    id_terms/2, % +IdSet:uid
                % -Terms:ordset(rdf_term)
    store_id/2, % +Term1:rdf_term
                % +Term2:rdf_term
    term_id/2, % +Term:rdf_term
               % -IdSet:uid
    term_term/2, % +Term1:rdf_term
                 % -Term2:rdf_term
    term_terms/2 % +Term:rdf_term
                 % -Terms:ordset(rdf_term)
  ]
).

/** <module> Identity store

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uuid_ext)).

:- rdf_meta(id_term(o,-)).
:- rdf_meta(id_terms(o,-)).
:- rdf_meta(term_id(o,-)).
:- rdf_meta(term_term(o,-)).
:- rdf_meta(term_terms(o,-)).

%! id_terms0(?IdSet:uid, ?Terms:ordset(rdf_term)) is nondet.

:- dynamic(id_terms0/2).

%! term_id0(?Term:rdf_term, IdSet:uid) is nondet.

:- dynamic(term_id0/2).





%! id_term(+IdSet:uid, -Term:rdf_term) is multi.

id_term(T, T):-
  rdf_is_literal0(T), !.
id_term(Id, T):-
  id_terms0(Id, Ts),
  member(T, Ts).



%! id_terms(+IdSet:uid, -Terms:ordset(rdf_term)) is multi.

id_terms(T, [T]):-
  rdf_is_literal0(T), !.
id_terms(Id, Ts):-
  id_terms0(Id, Ts).



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

term_id(T, _):-
  var(T), !.
term_id(T, T):-
  rdf_is_literal0(T), !.
term_id(T, TId):-
  term_id0(T, TId), !.
term_id(T, TId):-
  uuid_no_hyphen(TId),
  with_mutex(store_id, store_term0(T, TId)).


%! term_term(+Term1:rdf_term, -Term2:rdf_term) is multi.

term_term(T1, T2):-
  term_terms(T1, Ts),
  member(T2, Ts).



%! term_terms(+Term:rdf_term, -Terms:ordset(rdf_term)) is det.

term_terms(T, Ts):-
  term_id(T, Id),
  id_terms(Id, Ts).





% HELPERS %

%! rdf_is_literal0(@Term) is semidet.

rdf_is_literal0(T):-
  nonvar(T),
  T = literal(_).



%! store_term0(+Term:rdf_term, +TermId:uid) is det.

store_term0(T, TId):-
  assert(term_id0(T, TId)),
  assert(id_terms0(TId, [T])).
