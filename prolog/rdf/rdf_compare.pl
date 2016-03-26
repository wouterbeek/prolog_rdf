:- module(
  rdf_compare,
  [
    rdf_compare/3, % +X, +Y, +Pairs
    rdf_compare/5, % +X:rdf_term
                   % ?XGraph:rdf_graph
                   % +Y:rdf_term
                   % ?YGraph:rdf_graph
                   % +Pairs:ordset(pair(iri,list(ordset(rdf_term))))
    rdf_print_compare/2, % +X, +Y
    rdf_print_compare/4, % +X:rdf_term
                         % ?XGraph:rdf_graph
                         % +Y:rdf_term
                         % ?YGraph:rdf_graph
    rdf_shared_predicate/3, % +X, +Y, -Predicate
    rdf_shared_predicate/5, % +X:rdf_term
                            % +XGraph:rdf_graph
                            % +Y:rdf_term
                            % +YGraph:rdf_graph
                            % -Predicate:iri
    rdf_shared_predicates/3, % -X, -Y, +Predicates
    rdf_shared_predicates/5 % -X:rdf_term
                            % +XGraph:rdf_graph
                            % -Y:rdf_term
                            % +YGraph:rdf_graph
                            % +Predicates:ordset(iri)
  ]
).

/** <module> RDF compare

@author Wouter Beek
@version 2015/10-2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(default)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).


:- rdf_meta(rdf_compare(o,o,-)).
:- rdf_meta(rdf_compare(o,r,o,r,-)).
:- rdf_meta(rdf_print_compare(o,o)).
:- rdf_meta(rdf_print_compare(o,r,o,r)).
:- rdf_meta(rdf_shared_predicate(o,o,r)).
:- rdf_meta(rdf_shared_predicate(o,r,o,r,r)).
:- rdf_meta(rdf_shared_predicates(o,o,t)).
:- rdf_meta(rdf_shared_predicates(o,r,o,r,t)).





%! rdf_compare(
%!   +X:rdf_term,
%!   +XGraph:rdf_graph,
%!   +Y:rdf_term,
%!   +YGraph:rdf_graph,
%!   -Pairs:ordset(pair(iri,list(ordset(rdf_term))))
%! ) is det.

rdf_compare(X, Y, Pairs) :-
  rdf_compare(X, _, Y, _, Pairs).


%! rdf_compare(
%!   +X:rdf_term,
%!   +XGraph:rdf_graph,
%!   +Y:rdf_term,
%!   +YGraph:rdf_graph,
%!   -Pairs:ordset(pair(iri,list(ordset(rdf_term))))
%! ) is det.

rdf_compare(X, XG, Y, YG, SortedPairs) :-
  aggregate_all(set(Pair), rdf_xy(X, XG, Y, YG, Pair), SortedPairs).

rdf_xy(X, XG, Y, YG, P-[XYs,Xs,Ys]) :-
  distinct(P, (rdf(X, P, _, XG) ; rdf(Y, P, _, YG))),
  aggregate_all(set(O), (rdf(X, P, O, XG),    rdf(Y, P, O, YG)), XYs),
  aggregate_all(set(O), (rdf(X, P, O, XG), \+ rdf(Y, P, O, YG)), Xs),
  aggregate_all(set(O), (rdf(Y, P, O, YG), \+ rdf(X, P, O, XG)), Ys).



%! rdf_print_compare(+X:rdf_term, +Y:rdf_term) is det.

rdf_print_compare(X, Y) :-
  rdf_print_compare(X, _, Y, _).


%! rdf_print_compare(
%!   +X:rdf_term,
%!   ?XGraph:rdf_graph,
%!   +Y:rdf_term,
%!   ?YGraph:rdf_graph
%! ) is det.

rdf_print_compare(X, XG, Y, YG) :-
  rdf_compare(X, XG, Y, YG, Pairs),
  pairs_rows(Pairs, DataRows),
  HeadRow = head(['Predicate','XY','X','Y']),
  Opts = [caption(rdf_compare_caption0(X,Y)),cell(rdf_compare_cell0)],
  dcg_with_output_to(user_output, dcg_table([HeadRow|DataRows], Opts)).

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
  defval('', P),
  (selectchk(XY, XYs1, XYs2) -> true ; XY = '', XYs2 = XYs1),
  (selectchk(X, Xs1, Xs2)    -> true ; X  = '', Xs2  = Xs1 ),
  (selectchk(Y, Ys1, Ys2)    -> true ; Y  = '', Ys2  = Ys1 ),
  pair_rows(_, XYs2, Xs2, Ys2, T).

rdf_compare_caption0(X, Y) -->
  "Comparing terms ",
  dcg_print_term(X),
  " and ",
  dcg_print_term(Y).
rdf_compare_cell0(L) -->
  {is_list(L)}, !,
  set(dcg_print_term, L).
rdf_compare_cell0(T) -->
  dcg_print_term(T).



%! rdf_shared_predicate(+X:rdf_term, +Y:rdf_term, -Predicate:iri) is nondet.

rdf_shared_predicate(X, Y, P) :-
  rdf_shared_predicate(X, _, Y, _, P).


%! rdf_shared_predicate(
%!   +X:rdf_term,
%!   ?XGraph:rdf_graph,
%!   +Y:rdf_term,
%!   ?YGraph:rdf_graph,
%!   -Predicate:iri
%! ) is nondet.

rdf_shared_predicate(X, GX, Y, GY, P) :-
  rdf(X, P, Z, GX),
  rdf(Y, P, Z, GY).



%! rdf_shared_predicates(-X:rdf_term, -Y:rdf_term, +Predicates:ordset(iri)) is nondet.

rdf_shared_predicates(X, Y, Ps) :-
  rdf_shared_predicates(X, _, Y, _, Ps).


%! rdf_shared_predicates(
%!   -X:rdf_term,
%!   ?XGraph:rdf_graph,
%!   -Y:rdf_term,
%!   ?YGraph:rdf_graph,
%!   +Predicates:ordset(iri)
%! ) is nondet.

rdf_shared_predicates(X, GX, Y, GY, [P1|Ps]) :-
  rdf(X, P1, Z1, GX), findall(Z2, (member(P2, Ps), rdf(X, P2, Z2)), Zs),
  rdf(Y, P1, Z1, GY), findall(Z2, (member(P2, Ps), rdf(Y, P2, Z2)), Zs).
