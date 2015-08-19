:- module(
  j_db,
  [
    find_j/4, % ?Rule:compound
              % ?Premises:list(compound)
              % ?Conclusion:compound
              % ?Hash:atom
    print_j/4, % ?Rule:compound
               % ?Premises:list(compound)
               % ?Conclusion:compound
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
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(hash_ext)).
:- use_module(library(mat/mat_print)).

%! j(
%!   ?Rule:compound,
%!   ?Premises:list(atom),
%!   ?Conclusion:atom,
%!   ?Hash:atom
%! ) is nondet.

:- dynamic(j/4).

%! s(?Statement:compound, ?Hash:atom) is nondet.

:- dynamic(s/2).





%! find_j(
%!   ?Rule:compound,
%!   ?Predicates:list(compound),
%!   ?Conclusion:compound,
%!   ?Graph:atom
%! ) is nondet.

find_j(Rule, Ps0, C0, G):-
  var_or_md5(C0, C),
  (is_list(Ps0) -> maplist(var_or_md5, [C0|Ps0], [C|Ps]) ; true),
  j(Rule, Ps, C, G).

var_or_md5(X, X):- var(X), !.
var_or_md5(X, Y):- md5(X, Y).



%! print_j(
%!   ?Rule:compound,
%!   ?Predicates:list(compound),
%!   ?Conclusion:compound,
%!   ?Hash:atom
%! ) is nondet.

print_j(R, Ps, C, H):-
  find_j(R, Ps, C, H),
  dcg_with_output_to(current_output, print_deduction(R, Ps, C)).



%! store_j(
%!   +Rule:compound,
%!   +Predicates:list(compound),
%!   +Conclusion:compound
%! ) is det.

store_j(R, Ps, C):-
  maplist(store_s, [C|Ps], [HC|HPs]),
  md5(j(R, HPs, HC), HJ),
  store_j0(R, HPs, HC, HJ).

store_j0(_, _, _, HJ):-
  j(_, _, _, HJ), !.
store_j0(R, HPs, HC, HJ):-
  assert(j(R,HPs,HC,HJ)).

store_s(S, H):-
  md5(S, H),
  store_s0(S, H).

store_s0(S, H):-
  s(S, H), !.
store_s0(S, H):-
  assert(s(S,H)).
