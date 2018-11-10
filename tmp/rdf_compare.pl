:- module(
  rdf_compare,
  [
    rdf_comp/3,              % +X,      +Y,      +Pairs
    rdf_comp/5,              % +X, ?XG, +Y, ?YG, +Pairs
    rdf_print_comp/2,        % +X,      +Y
    rdf_print_comp/4,        % +X, ?XG, +Y, ?YG
    rdf_shared_predicate/3,  % +X,      +Y,      -P
    rdf_shared_predicate/5,  % +X, +XG, +Y, +YG, -P
    rdf_shared_predicates/3, % -X,      -Y,      +Ps
    rdf_shared_predicates/5  % -X, +XG, -Y, +YG, +Ps
  ]
).

/** <module> RDF compare

@author Wouter Beek
@version 2015/10-2015/12, 2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg)).
:- use_module(library(dcg_table)).
:- use_module(library(default)).
:- use_module(library(lists)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   rdf_comp(o, o, -),
   rdf_comp(o, r, o, r, -),
   rdf_print_comp(o, o),
   rdf_print_comp(o, r, o, r),
   rdf_shared_predicate(o, o, r),
   rdf_shared_predicate(o, r, o, r, r),
   rdf_shared_predicates(o, o, t),
   rdf_shared_predicates(o, r, o, r, t).





%! rdf_comp(+X, +Y, -Pairs) is det.
%! rdf_comp(+X, +XG, +Y, +YG, -Pairs) is det.

rdf_comp(X, Y, Pairs) :-
  rdf_comp(X, _, Y, _, Pairs).


rdf_comp(X, XG, Y, YG, SortedPairs) :-
  aggregate_all(set(Pair), rdf_xy0(X, XG, Y, YG, Pair), SortedPairs).


rdf_xy0(X, XG, Y, YG, P-[XYs,Xs,Ys]) :-
  distinct(P, (rdf(X, P, _, XG) ; rdf(Y, P, _, YG))),
  aggregate_all(set(O), (rdf(X, P, O, XG),    rdf(Y, P, O, YG)), XYs),
  aggregate_all(set(O), (rdf(X, P, O, XG), \+ rdf(Y, P, O, YG)), Xs),
  aggregate_all(set(O), (rdf(Y, P, O, YG), \+ rdf(X, P, O, XG)), Ys).



%! rdf_print_comp(+X, +Y) is det.
%! rdf_print_comp(+X, +XG, +Y, +YG) is det.

rdf_print_comp(X, Y) :-
  rdf_print_comp(X, _, Y, _).


rdf_print_comp(X, XG, Y, YG) :-
  rdf_comp(X, XG, Y, YG, Pairs),
  pairs_rows(Pairs, DataRows),
  HeadRow = head(['Predicate','XY','X','Y']),
  Opts = [caption(rdf_comp_caption0(X,Y)),cell(rdf_comp_cell0)],
  dcg_with_output_to(dcg_table([HeadRow|DataRows], Opts)).

pairs_rows(L1, L2) :-
  pairs_rows(L1, [], L2).

pairs_rows([H|T], L1, L) :- !,
  pair_rows(H, L2),
  append(L1, L2, L3),
  pairs_rows(T, L3, L).
pairs_rows([], L, L).

pair_rows(P-[XYs,Xs,Ys], L) :-
  pair_rows(P, XYs, Xs, Ys, L).

pair_rows(_, [], [], [], []) :- !.
pair_rows(P, XYs1, Xs1, Ys1, [[P,XY,X,Y]|T]) :-
  default_value(P, ''),
  (selectchk(XY, XYs1, XYs2) -> true ; XY = '', XYs2 = XYs1),
  (selectchk(X, Xs1, Xs2)    -> true ; X  = '', Xs2  = Xs1 ),
  (selectchk(Y, Ys1, Ys2)    -> true ; Y  = '', Ys2  = Ys1 ),
  pair_rows(_, XYs2, Xs2, Ys2, T).

rdf_comp_caption0(X, Y) -->
  "Comparing terms ",
  dcg_rdf_print_term(X),
  " and ",
  dcg_rdf_print_term(Y).
rdf_comp_cell0(L) -->
  {is_list(L)}, !,
  set(dcg_rdf_print_term, L).
rdf_comp_cell0(T) -->
  dcg_rdf_print_term(T).



%! rdf_shared_predicate(+X, +Y, -P) is nondet.
%! rdf_shared_predicate(+X, ?XG, +Y, ?YG, -P) is nondet.

rdf_shared_predicate(X, Y, P) :-
  rdf_shared_predicate(X, _, Y, _, P).


rdf_shared_predicate(X, GX, Y, GY, P) :-
  rdf(X, P, Z, GX),
  rdf(Y, P, Z, GY).



%! rdf_shared_predicates(-X, -Y, +Ps) is nondet.
%! rdf_shared_predicates(-X, ?XG, -Y, ?YG, +Ps) is nondet.

rdf_shared_predicates(X, Y, Ps) :-
  rdf_shared_predicates(X, _, Y, _, Ps).


rdf_shared_predicates(X, GX, Y, GY, [P1|Ps]) :-
  rdf(X, P1, Z1, GX), findall(Z2, (member(P2, Ps), rdf(X, P2, Z2)), Zs),
  rdf(Y, P1, Z1, GY), findall(Z2, (member(P2, Ps), rdf(Y, P2, Z2)), Zs).
