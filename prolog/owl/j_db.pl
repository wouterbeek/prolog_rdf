:- module(
  j_db,
  [
    j/4, % ?Rule:atom
         % ?Premises:list(atom)
         % ?Conclusion:atom
         % ?Hash:atom
    s/2, % ?Triple:compound
         % ?Hash:atom
    store_j/3 % +Rule:compound
              % +Predicates:list(compound)
              % +Conclusion:compound
  ]
).

/** <module> Justification database

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(hash_ext)).

%! j(
%!   ?Rule:atom,
%!   ?Premises:list(atom),
%!   ?Conclusion:atom,
%!   ?Hash:atom
%! ) is nondet.

:- dynamic(j/4).

%! s(?Triple:compound, ?Hash:atom) is nondet.

:- dynamic(s/2).





%! store_j(
%!   +Rule:compound,
%!   +Predicates:list(compound),
%!   +Conclusion:compound
%! ) is det.

store_j(Rule, Ps, C):-
  maplist(store_s, [C|Ps], [HC|HPs]),
  md5(j(Rule, HPs, HC), HJ),
  store_j0(Rule, HPs, HC, HJ).

store_j0(_, _, _, HJ):-
  j(_, _, _, HJ), !.
store_j0(Rule, HPs, HC, HJ):-
  assert(j(Rule,HPs,HC,HJ)).

store_s(T, H):-
  md5(T, H),
  store_s0(T, H).

store_s0(T, H):-
  s(T, H), !.
store_s0(T, H):-
  assert(s(T,H)).
