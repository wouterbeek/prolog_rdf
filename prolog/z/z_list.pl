:- module(
  z_list,
  [
    z_last/3,    % ?L, ?X, +G
    z_list/1,    % ?L
    z_list/2,    % ?L, ?G
    z_list_pl/2, % +RdfL, -PlL
    z_list_pl/3, % +RdfL, ?G, -PlL
    z_member/3   % ?X, ?L, +G
  ]
).

/** <module> Z list

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(z/z_stmt)).

:- rdf_meta
   z_last(r, r, r),
   z_list(r),
   z_list(r, r),
   z_list_pl(r, -),
   z_list_pl(r, r, -),
   z_member(r, r, r).





%! z_last(?L, ?X, +G) is nondet.

z_last(L, X, G) :-
  rdf_is_subject(L), !,
  z(L, rdf:rest, T, G),
  (   rdf_equal(T, rdf:nil)
  ->  z(L, rdf:first, X, G)
  ;   z_last(T, X, G)
  ).



%! z_list(?L) is nondet.
%! z_list(?L, ?G) is nondet.

z_list(L) :-
  z_list(L, _).


z_list(L, G) :-
  var(L), !,
  z(L, rdf:first, _, G),
  \+ z(_, rdf:rest, L, G),
  z_list0(L, G).
z_list(L, G) :-
  z_list0(L, G).


z_list0(rdf:nil, _) :- !.
z_list0(L, G) :-
  once(z(L, rdf:first, _, G)),
  z(L, rdf:rest, Rest, G),
  (rdf_equal(rdf:nil, Rest) -> true ; z_list0(Rest, G)).



%! z_list_pl(+RdfL, -PlL) is nondet.
%! z_list_pl(+RdfL, ?G, -PlL) is nondet.

z_list_pl(RdfL, PlL) :-
  z_list_pl(RdfL, _, PlL).


z_list_pl(RdfL, G, PlL) :-
  rdf_is_subject(RdfL), !,
  z_list_pl0(RdfL, G, PlL).
z_list_pl(RdfL, _, _) :-
  type_error(rdf_subject, RdfL).


:- rdf_meta  z_list_pl0(r, r, -).

z_list_pl0(rdf:nil, _, PlL) :- !,
  PlL = [].
z_list_pl0(RdfL, G, [H|T2]) :-
  (   z(RdfL, rdf:first, H0, G),
      z(RdfL, rdf:rest, T1, G)
  *-> H = H0,
      z_list_pl0(T1, G, T2)
  ;   type_error(rdf_list, RdfL)
  ).



%! z_member(?X, ?L, +G) is nondet.

z_member(X, L, G) :-
  ground(X), !,
  (   z_member2(X, L, G)
  ->  true
  ).
z_member(X, L, G) :-
  z_member2(X, L, G).


z_member2(X, L, G) :-
  z(L, rdf:first, X, G).
z_member2(X, L, G) :-
  z(L, rdf:rest, L0, G),
  z_member2(X, L0, G).
