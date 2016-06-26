:- module(
  z_list,
  [
    z_last/3,    % ?M, +RdfL, ?X
    z_last/4,    % ?M, +RdfL, ?X, ?G
    z_list/2,    % ?M, ?L
    z_list/3,    % ?M, ?L, ?G
    z_list_pl/3, % ?M, +RdfL, -PlL
    z_list_pl/4, % ?M, +RdfL, ?G, -PlL
    z_member/3,  % ?M, ?X, ?L
    z_member/4   % ?M, ?X, ?L, ?G
  ]
).

/** <module> Z list

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(z/z_stmt)).

:- rdf_meta
   z_last(?, r, r),
   z_last(?, r, r, r),
   z_list(?, r),
   z_list(?, r, r),
   z_list_pl(?, r, -),
   z_list_pl(?, r, r, -),
   z_member(?, r, r),
   z_member(?, r, r, r).





%! z_last(?M, +RdfL, ?X) is nondet.
%! z_last(?M, +RdfL, ?X, ?G) is nondet.

z_last(M, RdfL, X) :-
  z_last(M, RdfL, X, _).


z_last(M, RdfL, X, G) :-
  rdf_is_subject(RdfL), !,
  z(M, RdfL, rdf:rest, T, G),
  (   rdf_equal(T, rdf:nil)
  ->  z(M, RdfL, rdf:first, X, G)
  ;   z_last(M, T, X, G)
  ).
z_last(_, RdfL, _, _) :-
  type_error(rdf_subject, RdfL).



%! z_list(?M, ?L) is nondet.
%! z_list(?M, ?L, ?G) is nondet.

z_list(M, L) :-
  z_list(M, L, _).


z_list(M, L, G) :-
  var(L), !,
  z(M, L, rdf:first, _, G),
  \+ z(M, _, rdf:rest, L, G),
  z_list0(M, L, G).
z_list(M, L, G) :-
  z_list0(M, L, G).


z_list0(_, rdf:nil, _) :- !.
z_list0(M, L, G) :-
  once(z(M, L, rdf:first, _, G)),
  z(M, L, rdf:rest, Rest, G),
  (rdf_equal(rdf:nil, Rest) -> true ; z_list0(M, Rest, G)).



%! z_list_pl(?M, +RdfL, -PlL) is nondet.
%! z_list_pl(?M, +RdfL, ?G, -PlL) is nondet.

z_list_pl(M, RdfL, PlL) :-
  z_list_pl(M, RdfL, _, PlL).


z_list_pl(M, RdfL, G, PlL) :-
  rdf_is_subject(RdfL), !,
  z_list_pl0(M, RdfL, G, PlL).
z_list_pl(_, RdfL, _, _) :-
  type_error(rdf_subject, RdfL).


:- rdf_meta  z_list_pl0(?, r, r, -).

z_list_pl0(_, rdf:nil, _, PlL) :- !,
  PlL = [].
z_list_pl0(M, RdfL, G, [H|T2]) :-
  (   z(M, RdfL, rdf:first, H0, G),
      z(M, RdfL, rdf:rest, T1, G)
  *-> H = H0,
      z_list_pl0(M, T1, G, T2)
  ;   type_error(rdf_list, RdfL)
  ).



%! z_member(?M, ?X, ?L) is nondet.
%! z_member(?M, ?X, ?L, ?G) is nondet.

z_member(M, X, L) :-
  z_member(M, X, L, _).


z_member(M, X, L, G) :-
  ground(X), !,
  (   z_member2(M, X, L, G)
  ->  true
  ).
z_member(M, X, L, G) :-
  z_member2(M, X, L, G).


z_member2(M, X, L, G) :-
  z(M, L, rdf:first, X, G).
z_member2(M, X, L, G) :-
  z(M, L, rdf:rest, L0, G),
  z_member2(M, X, L0, G).
