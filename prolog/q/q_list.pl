:- module(
  q_list,
  [
    q_last/3,       % ?M, +RdfL, ?X
    q_last/4,       % ?M, +RdfL, ?X, ?G
    q_list/2,       % ?M, ?RdfL
    q_list/3,       % ?M, ?RdfL, ?G
    q_list/4,       % ?M, ?S, ?P, ?RdfL
    q_list/5,       % ?M, ?S, ?P, ?RdfL, ?G
    q_list_to_pl/3, % ?M, +RdfL, -PlL
    q_list_to_pl/4, % ?M, +RdfL, ?G, -PlL
    q_list_pl/4,    % ?M, ?S, ?P, -PlL
    q_list_pl/5,    % ?M, ?S, ?P, -PlL, ?G
    q_member/3,     % ?M, ?X, ?RdfL
    q_member/4      % ?M, ?X, ?RdfL, ?G
  ]
).

/** <module> Quine list API

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).

:- rdf_meta
   q_last(?, r, r),
   q_last(?, r, r, r),
   q_list(?, r),
   q_list(?, r, r),
   q_list(?, r, r, o),
   q_list(?, r, r, o, r),
   q_list_pl(?, r, r, -),
   q_list_pl(?, r, r, -, r),
   q_list_to_pl(?, r, r, -),
   q_list_to_pl(?, r, r, -, r),
   q_member(?, r, r),
   q_member(?, r, r, r).





%! q_last(?M, +RdfL, ?X) is nondet.
%! q_last(?M, +RdfL, ?X, ?G) is nondet.

q_last(M, RdfL, X) :-
  q_last(M, RdfL, X, _).


q_last(M, RdfL, X, G) :-
  q_is_subject(RdfL), !,
  q(M, RdfL, rdf:rest, T, G),
  (   rdf_equal(T, rdf:nil)
  ->  q(M, RdfL, rdf:first, X, G)
  ;   q_last(M, T, X, G)
  ).
q_last(_, RdfL, _, _) :-
  type_error(rdf_subject, RdfL).



%! q_list(?M, ?RdfL) is nondet.
%! q_list(?M, ?RdfL, ?G) is nondet.
%! q_list(?M, ?S, ?P, ?RdfL) is nondet.
%! q_list(?M, ?S, ?P, ?RdfL, ?G) is nondet.

q_list(M, RdfL) :-
  q_list(M, RdfL, _).


q_list(M, RdfL, G) :-
  var(RdfL), !,
  q(M, RdfL, rdf:first, _, G),
  \+ q(M, _, rdf:rest, RdfL, G),
  q_list0(M, RdfL, G).
q_list(M, RdfL, G) :-
  q_list0(M, RdfL, G).


q_list0(_, rdf:nil, _) :- !.
q_list0(M, RdfL, G) :-
  once(q(M, RdfL, rdf:first, _, G)),
  q(M, RdfL, rdf:rest, Rest, G),
  (rdf_equal(rdf:nil, Rest) -> true ; q_list0(M, Rest, G)).


q_list(M, S, P, RdfL) :-
  q(M, S, P, RdfL),
  q_list(M, RdfL).


q_list(M, S, P, RdfL, G) :-
  q(M, S, P, RdfL, G),
  q_list(M, RdfL, G).



%! q_list_to_pl(?M, +RdfL, -PlL) is nondet.
%! q_list_to_pl(?M, +RdfL, ?G, -PlL) is nondet.
%! q_list_pl(?M, ?S, ?P, -PlL) is nondet.
%! q_list_pl(?M, ?S, ?P, ?G, -PlL, ) is nondet.

q_list_to_pl(M, RdfL, PlL) :-
  q_list_to_pl(M, RdfL, _, PlL).


q_list_to_pl(M, RdfL, G, PlL) :-
  q_is_subject(RdfL), !,
  q_list_to_pl0(M, RdfL, G, PlL).
q_list_to_pl(_, RdfL, _, _) :-
  type_error(rdf_subject, RdfL).


q_list_pl(M, S, P, PlL) :-
  q_list_pl(M, S, P, PlL, _).


q_list_pl(M, S, P, PlL, G) :-
  q_list(M, S, P, RdfL, G),
  q_list_to_pl(M, RdfL, PlL).


:- rdf_meta
   q_list_to_pl0(?, r, r, -).

q_list_to_pl0(_, rdf:nil, _, PlL) :- !,
  PlL = [].
q_list_to_pl0(M, RdfL, G, [H|T2]) :-
  (   q(M, RdfL, rdf:first, H0, G),
      q(M, RdfL, rdf:rest, T1, G)
  *-> H = H0,
      q_list_to_pl0(M, T1, G, T2)
  ;   type_error(rdf_list, RdfL)
  ).



%! q_member(?M, ?X, ?RdfL) is nondet.
%! q_member(?M, ?X, ?RdfL, ?G) is nondet.

q_member(M, X, RdfL) :-
  q_member(M, X, RdfL, _).


q_member(M, X, RdfL, G) :-
  ground(X), !,
  (   q_member2(M, X, RdfL, G)
  ->  true
  ).
q_member(M, X, RdfL, G) :-
  q_member2(M, X, RdfL, G).


q_member2(M, X, RdfL, G) :-
  q(M, RdfL, rdf:first, X, G).
q_member2(M, X, RdfL, G) :-
  q(M, RdfL, rdf:rest, RdfL0, G),
  q_member2(M, X, RdfL0, G).
