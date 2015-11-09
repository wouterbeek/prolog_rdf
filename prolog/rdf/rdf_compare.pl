:- module(
  rdf_compare,
  [
    rdf_compare/3, % +X:rdf_term
                   % +Y:rdf_term
                   % +Relation:oneof([<,=,>])
    rdf_compare/6 % +X:rdf_term
                  % +Y:rdf_term
                  % +Relation:oneof([<,=,>])
                  % -XOnly:ordset(compound)
                  % -XYDiff:ordset(compound)
                  % -YOnly:ordset(compound)
  ]
).

/** <module> RDF compare

@author Wouter Beek
@version 2015/10-2015/11
*/

:- use_module(library(aggregate)).
:- use_module(library(owl/owl_api)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_compare(o,o,+)).
:- rdf_meta(rdf_compare(o,o,+,-,-,-)).





%! rdf_compare(+X:rdf_term, +Y:rdf_term, +Relation:oneof([<,=,>])) is semidet.

rdf_compare(X, Y, Rel):-
  rdf_compare(X, Y, Rel, _, _, _).


%! rdf_compare(
%!   +X:rdf_term,
%!   +Y:rdf_term,
%!   +Relation:oneof([<,=,>]),
%!   -XOnly:ordset(compound),
%!   -XYDiff:ordset(compound),
%!   -YOnly:ordset(compound)
%! ) is det.

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
  aggregate_all(set(rdf(X,P,O)), user:rdf(X, P, O), Xs),
  aggregate_all(set(rdf(Y,P,O)), user:rdf(Y, P, O), Ys),
  rdf_compare_split0(Xs, Ys, XOnly, XYDiff, YOnly).

% Y must be ahead of X.
rdf_compare_split0([X|Xs], [Y|Ys], XOnly, XYDiff, YOnly):-
  rdf_ahead_of0(X, Y), !,
  rdf_compare_split0([Y|Ys], [X|Xs], YOnly, XYDiff, XOnly).
% Y is ahead of X.
rdf_compare_split0([_X|_Xs], [Y|Ys], _XOnly, _XYDiff, _YOnly):-
  selectchk(rdf(Y,_P,_O), Ys, Ys0).

rdf_ahead_of0(rdf(_,P1,_), rdf(_,P2,_)):- P1 @> P2, !.
rdf_ahead_of0(rdf(_,P1,O1), rdf(_,P2,O2)):- P1 =@= P2, O1 @> O2.
