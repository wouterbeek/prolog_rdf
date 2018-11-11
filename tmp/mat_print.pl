:- module(
  mat_print,
  [
    print_deduction//3, % +Rule, +Prems, +Conc
    print_rule//1 % +Rule
  ]
).

/** <module> Materialization print

Printing of materialization results.

@author Wouter Beek
@version 2015/08-2015/12, 2016/03, 2016/08
*/

:- use_module(library(dcg)).
:- use_module(library(dcg_pl)).
:- use_module(library(dcg_unicode)).
:- use_module(library(html/html_ext)).
:- use_module(library(q/rdf_print)).





%! print_conclusion(+Conc)// is det.

print_conclusion(Conc) -->
  "  ",
  provable,
  "   ",
  print_expression0(Conc),
  nl.



%! print_deduction(+Rule, +Prems, +Conc)// is det.

print_deduction(Rule, Prems, Conc) -->
  "[", print_rule(Rule), "]",
  nl,
  print_premises(Prems),
  print_conclusion(Conc).



%! print_expression0(+Expression:compound)// is det.

print_expression0(error) --> !,
  falsum.
print_expression0(rdf(S,P,O)) --> !,
  rdf_dcg_tp(S, P, O).
print_expression0(T) -->
  pl_term(T).



%! print_premise(+Index, +P)// is det.

print_premise(I, P) -->
  "P",
  thousands(I),
  ": ",
  print_expression0(P).



%! print_premises(+Prems)// is det.
% Wrapper around print_premises//2.

print_premises(Ps) -->
  print_premises(1, Ps).


%! print_premises(+Index, +Prems)// is det.

print_premises(I1, [P|Ps]) --> !,
  "  ",
  print_premise(I1, P),
  nl,
  {I2 is I1 + 1},
  print_premises(I2, Ps).
print_premises(_, []) --> [].



%! print_rule(+Rule:compound)// is det.

print_rule(R) -->
  {unwind_compound(R, L)},
  ({L == []} -> "" ; {L = [H|T]}, atom(H), +(sep_atom, T)).
unwind_compound(H, [H]) :- atomic(H), !.
unwind_compound(H0, [H|T]) :- H0 =.. [H,T0], unwind_compound(T0, T).
sep_atom(X) --> ",", atom(X).
