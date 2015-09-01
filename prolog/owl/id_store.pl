:- module(
  id_store,
  [
    id_term/2, % +IdSet:uid
               % -Term:rdf_term
    id_terms/2, % +IdSet:uid
                % -Terms:ordset(rdf_term)
    print_id_store/0,
    print_id_store/1, % +Options:list(compound)
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

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(id_term(o,-)).
:- rdf_meta(id_terms(o,-)).
:- rdf_meta(term_id(o,-)).
:- rdf_meta(term_term(o,-)).
:- rdf_meta(term_terms(o,-)).

%! id_terms0(?IdSet:uid, ?Terms:ordset(rdf_term)) is nondet.

:- dynamic(id_terms0/2).

%! term_id0(?Term:rdf_term, IdSet:uid) is nondet.

:- dynamic(term_id0/2).

:- dynamic(id_counter/1).
id_counter(0).

:- predicate_options(print_id_store/1, 1, [
     pass_to(rdf_print_term//2, 2)
   ]).





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



%! print_id_store is det.
% Wrapper around print_id_store/1 with default options.

print_id_store:-
  print_id_store([]).

%! print_id_store(+Options:list(compound)) is det.
% The following options are supported:
%   * indent(+nonneg)
%     Default is 0.

print_id_store(Opts):-
  option(indent(N), Opts, 0),
  forall(
    id_terms0(Id, Ts),
    dcg_with_output_to(user_output, (
      indent(N),
      atom(Id),
      "\t",
      set(\T^rdf_print_term(T, Opts), Ts),
      nl
   ))
  ).



%! store_id(+X:rdf_term, +Y:rdf_term) is det.

store_id(X, Y):-
  (   term_id0(X, XId)
  ->  (   term_id0(Y, YId)
      ->  (   % X and Y already belong to the same identity set.
              XId == YId
          ->  true
          ;   % Merge the identity sets of X and Y.
              id_terms0(XId, Xs),
              id_terms0(YId, Ys),
              ord_union(Xs, Ys, Zs),
              fresh_id(ZId),
              with_mutex(store_id, (
                retract(term_id0(X, XId)),
                retract(id_terms0(XId, Xs)),
                retract(term_id0(Y, YId)),
                retract(id_terms0(YId, Ys)),
                assert(term_id0(X, ZId)),
                assert(term_id0(Y, ZId)),
                assert(id_terms0(ZId, Zs)),
                rdf_rename_term(XId, ZId),
                rdf_rename_term(YId, ZId)
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
  ;   term_id0(Y, YId)
  ->  % Add X to the identity set of Y.
      id_terms0(YId, Ys1),
      ord_add_element(Ys1, X, Ys2),
      with_mutex(store_id, (
        retract(id_terms0(YId, Ys1)),
        assert(id_terms0(YId, Ys2)),
        assert(term_id0(X, YId))
      ))
  ;   fresh_id(XId),
      fresh_id(YId),
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
  fresh_id(TId),
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

%! fresh_id(-Id:nonneg) is det.

fresh_id(Id):-
  with_mutex(store_id, (
    retract(id_counter(M)),
    succ(M, N),
    assert(id_counter(N))
  )),
  atom_number(Id, M).



%! rdf_is_literal0(@Term) is semidet.

rdf_is_literal0(T):-
  nonvar(T),
  T = literal(_).



%! rdf_rename_term(+From, +To) is det.

rdf_rename_term(X, Y):-
  forall(rdf(X, P, O), rdf_assert(Y, P, O)),
  rdf_retractall(X, _, _),
  forall(rdf(S, X, O), rdf_assert(S, Y, O)),
  rdf_retractall(_, X, _),
  forall(rdf(S, P, X), rdf_assert(S, P, Y)),
  rdf_retractall(_, _, X).
  


%! store_term0(+Term:rdf_term, +TermId:uid) is det.

store_term0(T, TId):-
  assert(term_id0(T, TId)),
  assert(id_terms0(TId, [T])).
