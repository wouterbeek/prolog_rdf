:- use_module(owl/owl_api)).

rdf_compare(X, Y, Rel):-
  rdf_compare(X, Y, Rel, _, _, _).

rdf_compare(X, Y, Rel, XOnly, XYDiff, YOnly):-
  rdf_compare0(X, Y, XOnly, XYDiff, YOnly),
  (   XYDiff == []
  ->  (   XOnly == [],
          YOnly \== []
      ->  Rel = <
      ;   XOnly \== [],
          YOnly == []
      ->  Rel = >
      ;   XOnly == [],
          YOnly == []
      ->  Rel = =
      ;   true
      )
  ;   true
  ).

rdf_compare0(X, Y, XOnly, XYDiff, YOnly):-
  aggregate_all(set(rdf(X,P,O)), rdf3(X, P, O), Xs),
  aggregate_all(set(rdf(Y,P,O)), rdf3(Y, P, O), Ys),
  rdf_compare_split0(Xs, Ys, XOnly, XYDiff, YOnly).

% Y must be ahead of X.
rdf_compare_split0([X|Xs], [Y|Ys], XOnly, XYDiff, YOnly):-
  rdf_ahead_of0(X, Y), !,
  rdf_compare_split0([Y|Ys], [X|Xs], YOnly, XYDiff, XOnly).
% Y is ahead of X.
rdf_compare_split0([X|Xs], [Y|Ys], XOnly, XYDiff, YOnly):-
  select_chk(rdf(Y,P,O), Ys, Ys0),

rdf_ahead_of0(rdf(_,P1,O1), rdf(_,P2,O2)):- P1 @> P2, !.
rdf_ahead_of0(rdf(_,P1,O1), rdf(_,P2,O2)):- P1 @= P2, O1 @> O2.
