:- module(
  owl_mat_deb,
  [
    owl_mat_deb/3
  ]
).

/** <module> Debug for materialization

Debug tools for calculating a materialization.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_logic)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug)).
:- use_module(library(owl/j_db)).
:- use_module(library(rdf/rdf_print)).



%! owl_mat_deb(+Rule:compound, +Conclusion:compound) is det.

owl_mat_deb(Rule, C):-
  (   debugging(mat(Rule))
  ->  rule_label(Rule, Label),
      string_phrase(print_idle(Label, C), S),
      debug(mat(Rule), '~s', S)
  ;   true
  ).

print_idle(Rule, rdf(S,P,O)) -->
  bracketed(square, atom(Rule)),
  "\n  ",
  rdf_print:rdf_print_statement(S, P, O, _, [logic_sym(true)]).


%! owl_mat_deb(
%!   +Rule:compound,
%!   +Predicates:list(compound),
%!   +Conclusion:compound
%! ) is det.

owl_mat_deb(Rule, Ps, C):-
  store_j(Rule, Ps, C),
  (   debugging(mat(Rule))
  ->  rule_label(Rule, Label),
      string_phrase(print_deduction(Label, Ps, C, [logic_sym(true)]), S),
      debug(mat(Rule), '~s', S)
  ;   true
  ).

rule_label(Rule, Label):-
  unwind_compound(Rule, Label0),
  atomic_list_concat(Label0, ':', Label).

unwind_compound(H, [H]):-
  atomic(H), !.
unwind_compound(H0, [H|T]):-
  H0 =.. [H,T0],
  unwind_compound(T0, T).

print_deduction(Rule, Ps, C, Opts) -->
  bracketed(square, atom(Rule)),
  "\n",
  print_premises(1, Ps, Opts),
  print_conclusion(C, Opts).

print_premises(_, [], _) --> "", !.
print_premises(N1, [H|T], Opts) -->
  "  ",
  print_premise(N1, H, Opts),
  "\n",
  {succ(N1, N2)},
  print_premises(N2, T, Opts).

print_premise(N, rdf(S,P,O), Opts) -->
  "P",
  integer(N),
  ": ",
  rdf_print:rdf_print_statement(S, P, O, _, Opts).

print_conclusion(rdf(S, P, O), Opts) -->
  "  ",
  provable,
  "   ",
  rdf_print:rdf_print_statement(S, P, O, _, Opts).
