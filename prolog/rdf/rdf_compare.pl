:- module(
  rdf_compare,
  [
    rdf_compare/2, % +X, +Y
    rdf_compare/3 % +X:rdf_term
                  % +Y:rdf_term
                  % +Pairs:ordset(pair(iri,list(ordset(rdf_term))))
  ]
).

/** <module> RDF compare

@author Wouter Beek
@version 2015/10-2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(default)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(solution_sequences)).

:- rdf_meta(rdf_compare(o,o)).
:- rdf_meta(rdf_compare(o,o,-)).





%! rdf_compare(+X:rdf_term, +Y:rdf_term) is det.

rdf_compare(X, Y):-
  rdf_compare(X, Y, Pairs),
  pairs_rows(Pairs, DataRows),
  HeadRow = head(['Predicate','XY','X','Y']),
  Opts = [caption(rdf_compare_caption0(X,Y)),cell(rdf_compare_cell0)],
  dcg_with_output_to(user_output, dcg_table([HeadRow|DataRows], Opts)).

pairs_rows(L1, L2):-
  pairs_rows(L1, [], L2).

pairs_rows([H|T], L1, L):- !,
  pair_rows(H, L2),
  append(L1, L2, L3),
  pairs_rows(T, L3, L).
pairs_rows([], L, L).

pair_rows(P-[XYs,Xs,Ys], L):-
  pair_rows(P, XYs, Xs, Ys, L).

pair_rows(_, [], [], [], []):- !.
pair_rows(P, XYs1, Xs1, Ys1, [[P,XY,X,Y]|T]):-
  defval('', P),
  (selectchk(XY, XYs1, XYs2) -> true ; XY = '', XYs2 = XYs1),
  (selectchk(X, Xs1, Xs2)    -> true ; X  = '', Xs2  = Xs1 ),
  (selectchk(Y, Ys1, Ys2)    -> true ; Y  = '', Ys2  = Ys1 ),
  pair_rows(_, XYs2, Xs2, Ys2, T).

rdf_compare_caption0(X, Y) -->
  "Comparing terms ",
  rdf_print_term(X),
  " and ",
  rdf_print_term(Y).
rdf_compare_cell0(L) -->
  {is_list(L)}, !,
  set(rdf_print_term, L).
rdf_compare_cell0(T) -->
  rdf_print_term(T).


%! rdf_compare(
%!   +X:rdf_term,
%!   +Y:rdf_term,
%!   -Pairs:ordset(pair(iri,list(ordset(rdf_term))))
%! ) is det.

rdf_compare(X, Y, SortedPairs):-
  aggregate_all(set(Pair), rdf_xy(X, Y, Pair), SortedPairs).

rdf_xy(X, Y, P-[XYs,Xs,Ys]):-
  distinct(P, (rdf(X, P, _) ; rdf(Y, P, _))),
  aggregate_all(set(O), (rdf(X, P, O),    rdf(Y, P, O)), XYs),
  aggregate_all(set(O), (rdf(X, P, O), \+ rdf(Y, P, O)), Xs),
  aggregate_all(set(O), (rdf(Y, P, O), \+ rdf(X, P, O)), Ys).
