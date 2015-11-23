:- module(
  id_store,
  [
    assign_term_id/2, % ?Term:rdf_term
                      % ?IdSet:uri
    id_terms/1, % ?Terms:ordset(rdf_term)
    id_to_term/2, % +IdSet:uid
                  % -Term:rdf_term
    id_to_terms/2, % +IdSet:uid
                   % -Terms:ordset(rdf_term)
    print_id_store/0,
    print_id_store/1, % +Options:list(compound)
    remove_id/1, % +IdSet:uid
    store_id/1, % +Terms:list(rdf_term)
    store_id/2, % +Term1:rdf_term
                % +Term2:rdf_term
    term_to_id/2, % +Term:rdf_term
                  % -IdSet:uri
    term_to_term/2, % +Term1:rdf_term
                    % -Term2:rdf_term
    term_to_terms/2, % +Term:rdf_term
                     % -Terms:ordset(rdf_term)
    var_or_term_to_id/2 % ?Term:rdf_term
                        % ?IdSet:uid
  ]
).

/** <module> Identity store

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(error)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(assign_term_id(o,?)).
:- rdf_meta(id_terms(t)).
:- rdf_meta(store_id(t)).
:- rdf_meta(store_id(o,o)).
:- rdf_meta(term_to_id(o,-)).
:- rdf_meta(term_to_term(o,-)).
:- rdf_meta(term_to_terms(o,-)).
:- rdf_meta(var_or_term_to_id(o,?)).

%! id_terms0(?IdSet:uid, ?Terms:ordset(rdf_term)) is nondet.

:- dynamic(id_terms0/2).

%! term_id0(?Term:rdf_term, IdSet:uid) is nondet.

:- dynamic(term_id0/2).

:- dynamic(id_counter/1).
id_counter(0).

:- predicate_options(print_id_store/1, 1, [
     pass_to(rdf_print_term//2, 2)
   ]).





%! assign_term_id(?Term:rdf_term, -IdSet:uid) is det.

assign_term_id(T, TId):-
  var_or_term_to_id(T, TId), !.
assign_term_id(T, TId):-
  fresh_id(TId),
  with_mutex(store_id, store_term0(T, TId)).



%! id_terms(+Terms:ordset(rdf_term)) is semidet.
%! id_terms(-Terms:ordset(rdf_term)) is nondet.

id_terms(Ts1):-
  nonvar(Ts1), !,
  memberchk(T, Ts1),
  term_id0(T, TId),
  id_terms0(TId, Ts2),
  ord_subset(Ts1, Ts2).
id_terms(Ts):-
  id_terms0(_, Ts).



%! id_to_term(+IdSet:uid, +Term:rdf_term) is multi.

id_to_term(Id, T):-
  nonvar(Id), !,
  id_to_terms(Id, Ts),
  member(T, Ts).
id_to_term(Id, T):-
  nonvar(T), !,
  term_id0(T, Id).
id_to_term(Id, T):-
  instantiation_error(id_term(Id,T)).



%! id_to_terms(+IdSet:uid, -Terms:ordset(rdf_term)) is multi.

id_to_terms(Id, Ts):-
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
      tab(N),
      atom(Id),
      "\t",
      set(\T^rdf_print_term(T, Opts), Ts),
      nl
   ))
  ).



%! remove_id(+Id:uid) is det.

remove_id(Id):-
  with_mutex(id_store, (
    retractall(id_terms0(Id,_)),
    retractall(term_id0(_,Id))
  )).




%! store_id(+Terms:list(rdf_term)) is det.

store_id([H1,H2|T]):-
  store_id(H1, H2), !,
  store_id([H2|T]).
store_id([_]):- !.
store_id([]).


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



%! term_to_id(+Term:rdf_term, -IdSet:uid) is det.

term_to_id(T, Id):-
  term_id0(T, Id).



%! term_to_term(+Term1:rdf_term, -Term2:rdf_term) is multi.

term_to_term(T1, T2):-
  term_to_terms(T1, Ts),
  member(T2, Ts).



%! term_to_terms(+Term:rdf_term, -Terms:ordset(rdf_term)) is det.

term_to_terms(T, Ts):-
  term_to_id(T, Id),
  id_to_terms(Id, Ts).



%! var_or_term_to_id(?Term:rdf_term, ?IdSet:uid) is det.

var_or_term_to_id(T, _):-
  var(T), !.
var_or_term_to_id(T, TId):-
  term_to_id(T, TId).





% HELPERS %

%! fresh_id(-Id:nonneg) is det.

fresh_id(Id):-
  with_mutex(store_id, (
    retract(id_counter(M)),
    succ(M, N),
    assert(id_counter(N))
  )),
  atom_number(Id, M).



%! rdf_rename_term(+From, +To) is det.

rdf_rename_term(X, Y):-
  forall(rdf_db:rdf(X, P, O), rdf_db:rdf_assert(Y, P, O)),
  rdf_db:rdf_retractall(X, _, _),
  forall(rdf_db:rdf(S, X, O), rdf_db:rdf_assert(S, Y, O)),
  rdf_db:rdf_retractall(_, X, _),
  forall(rdf_db:rdf(S, P, X), rdf_db:rdf_assert(S, P, Y)),
  rdf_db:rdf_retractall(_, _, X).
  


%! store_term0(+Term:rdf_term, +TermId:uid) is det.

store_term0(T, TId):-
  assert(term_id0(T, TId)),
  assert(id_terms0(TId, [T])).
