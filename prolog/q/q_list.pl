:- module(
  q_list,
  [
    q_last/3,    % ?M, +RdfL, ?X
    q_last/4,    % ?M, +RdfL, ?X, ?G
    q_list/2,    % ?M, ?L
    q_list/3,    % ?M, ?L, ?G
    q_list_pl/3, % ?M, +RdfL, -PlL
    q_list_pl/4, % ?M, +RdfL, ?G, -PlL
    q_member/3,  % ?M, ?X, ?L
    q_member/4   % ?M, ?X, ?L, ?G
  ]
).

/** <module> Quine list API

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(q/q_stmt)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   q_last(?, r, r),
   q_last(?, r, r, r),
   q_list(?, r),
   q_list(?, r, r),
   q_list_pl(?, r, -),
   q_list_pl(?, r, r, -),
   q_member(?, r, r),
   q_member(?, r, r, r).





%! q_last(?M, +RdfL, ?X) is nondet.
%! q_last(?M, +RdfL, ?X, ?G) is nondet.

q_last(M, RdfL, X) :-
  q_last(M, RdfL, X, _).


q_last(M, RdfL, X, G) :-
  rdf_is_subject(RdfL), !,
  q(M, RdfL, rdf:rest, T, G),
  (   rdf_equal(T, rdf:nil)
  ->  q(M, RdfL, rdf:first, X, G)
  ;   q_last(M, T, X, G)
  ).
q_last(_, RdfL, _, _) :-
  type_error(rdf_subject, RdfL).



%! q_list(?M, ?L) is nondet.
%! q_list(?M, ?L, ?G) is nondet.

q_list(M, L) :-
  q_list(M, L, _).


q_list(M, L, G) :-
  var(L), !,
  q(M, L, rdf:first, _, G),
  \+ q(M, _, rdf:rest, L, G),
  q_list0(M, L, G).
q_list(M, L, G) :-
  q_list0(M, L, G).


q_list0(_, rdf:nil, _) :- !.
q_list0(M, L, G) :-
  once(q(M, L, rdf:first, _, G)),
  q(M, L, rdf:rest, Rest, G),
  (rdf_equal(rdf:nil, Rest) -> true ; q_list0(M, Rest, G)).



%! q_list_pl(?M, +RdfL, -PlL) is nondet.
%! q_list_pl(?M, +RdfL, ?G, -PlL) is nondet.

q_list_pl(M, RdfL, PlL) :-
  q_list_pl(M, RdfL, _, PlL).


q_list_pl(M, RdfL, G, PlL) :-
  rdf_is_subject(RdfL), !,
  q_list_pl0(M, RdfL, G, PlL).
q_list_pl(_, RdfL, _, _) :-
  type_error(rdf_subject, RdfL).


:- rdf_meta  q_list_pl0(?, r, r, -).

q_list_pl0(_, rdf:nil, _, PlL) :- !,
  PlL = [].
q_list_pl0(M, RdfL, G, [H|T2]) :-
  (   q(M, RdfL, rdf:first, H0, G),
      q(M, RdfL, rdf:rest, T1, G)
  *-> H = H0,
      q_list_pl0(M, T1, G, T2)
  ;   type_error(rdf_list, RdfL)
  ).



%! q_member(?M, ?X, ?L) is nondet.
%! q_member(?M, ?X, ?L, ?G) is nondet.

q_member(M, X, L) :-
  q_member(M, X, L, _).


q_member(M, X, L, G) :-
  ground(X), !,
  (   q_member2(M, X, L, G)
  ->  true
  ).
q_member(M, X, L, G) :-
  q_member2(M, X, L, G).


q_member2(M, X, L, G) :-
  q(M, L, rdf:first, X, G).
q_member2(M, X, L, G) :-
  q(M, L, rdf:rest, L0, G),
  q_member2(M, X, L0, G).
