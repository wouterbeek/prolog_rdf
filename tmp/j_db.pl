:- module(
  j_db,
  [
    clear_j/0,
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
@version 2015/08, 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg)).
:- use_module(library(dlist)).
:- use_module(library(hash_ext)).
:- use_module(library(mat/mat_print)).
:- use_module(library(ordsets)).

%! j(
%!   ?Rule:compound,
%!   ?Premises:list(md5),
%!   ?Conclusion:md5,
%!   ?Justification:md5
%! ) is nondet.
% Justifications are rule-based deductions from premises to a conclusion.
% Premises and conclusions are hashed according to dictionary s/2.

:- dynamic(j/4).

%! s(?Statement:compound, ?Hash:md5) is nondet.
% Statements are hashed.

:- dynamic(s/2).





%! clear_j is det.

clear_j:-
  retractall(j(_,_,_,_)),
  retractall(s(_,_)).



%! find_j(
%!   ?Rule:compound,
%!   ?Predicates:list(compound),
%!   ?Conclusion:compound,
%!   -Justification:md5
%! ) is nondet.
% Search for justifications based on

find_j(Rule, Ps, C, J) :-
  nonvar(J), !,
  j(Rule, Ps, C, J).
find_j(Rule, Ps0, C0, J) :-
  % Lookup the predicates in s/2 to the extent that they are instantiated.
  (   var(Ps0)
  ->  Ps1 = []
  ;   is_list(Ps0)
  ->  maplist(var_or_md5, Ps0, Ps1)
  ),
  list_to_ord_set(Ps1, Ps2),

  % Lookup the conclusion in s/2 if it is instantiated.
  var_or_md5(C0, C),

  % Performs subset matching on the predicates.
  j(Rule, Ps3, C, J),
  ord_subset(Ps2, Ps3).

var_or_md5(X, X) :- var(X), !.
var_or_md5(X, Y) :- md5(X, Y).



%! print_j(
%!   ?Rule:compound,
%!   ?Predicates:list(compound),
%!   ?Conclusion:compound,
%!   ?Justification:md5
%! ) is nondet.

print_j(Rule0, Ps0, C0, J) :-
  % NONDET
  find_j(Rule0, Ps0, C0, J),
  j(Rule, Ps, C, J),
  dcg_with_output_to(print_deduction(Rule, Ps, C)).



%! store_j(
%!   +Rule:compound,
%!   +Predicates:list(compound),
%!   +Conclusion:compound
%! ) is det.
% Stores a justification.

store_j(R, Ps, C) :-
  % Hash all statements in dictionary s/2.
  maplist(store_s, [C|Ps], [HC|HPs]),
  % Hash the justification.
  md5(j(R, HPs, HC), HJ),
  store_j0(R, HPs, HC, HJ).

store_j0(_, _, _, HJ) :-
  j(_, _, _, HJ), !.
store_j0(R, HPs, HC, HJ) :-
  assert(j(R,HPs,HC,HJ)).



%! store_s(+Statement:compound, -Hash:md5) is det.
% Stores a given Statements and returns the Hash under which it can be found.

store_s(S, H) :-
  md5(S, H),
  store_s0(S, H).

store_s0(S, H) :-
  s(S, H), !.
store_s0(S, H) :-
  assert(s(S,H)).
