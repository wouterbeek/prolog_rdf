:- module(
  mat_print,
  [
    print_deduction//3, % +Rule:compound
                        % +Premises:list(compound)
                        % +Conclusion:compond
    print_deduction//4, % +Rule:compound
                        % +Premises:list(compound)
                        % +Conclusion:compond
                        % +Options:list(compound)
    print_rule//1 % +Rule:compound
  ]
).

/** <module> Materialization print

Printing of materialization results.

@author Wouter Beek
@version 2015/08-2015/09
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_pl_term)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(rdf/rdf_print_stmt)).

:- predicate_options(print_conclusion//2, 2, [
     pass_to(print_expression0//2, 2)
   ]).
:- predicate_options(print_deduction//3, 3, [
     pass_to(print_deduction//4, 4)
   ]).
:- predicate_options(print_deduction//4, 4, [
     pass_to(print_conclusion//2, 2),
     pass_to(print_premises//2, 2)
   ]).
:- predicate_options(print_premise//3, 3, [
     pass_to(print_expression0//2, 2)
   ]).
:- predicate_options(print_premises//2, 2, [
     pass_to(print_premises//3, 3)
   ]).
:- predicate_options(print_premises//3, 3, [
     pass_to(print_premise//3, 3)
   ]).
:- predicate_options(print_expression0//2, 2, [
     pass_to(rdf_print_statement//5, 5)
   ]).





%! print_conclusion(+Conclusion:compound, +Options:list(compound))// is det.

print_conclusion(T, Opts) -->
  "  ",
  provable,
  "   ",
  print_expression0(T, Opts),
  nl.



%! print_deduction(
%!   +Rule:compound,
%!   +Premises:list(compound),
%!   +Conclusion:compond
%! )// is det.
% Wrapper around print_deduction//4 with default options.

print_deduction(R, Ps, C) -->
  print_deduction(R, Ps, C, []).


%! print_deduction(
%!   +Rule:compound,
%!   +Premises:list(compound),
%!   +Conclusion:compond,
%!   +Options:list(compound)
%! )// is det.

print_deduction(R, Ps, C, Opts) -->
  "[", print_rule(R), "]",
  nl,
  print_premises(Ps, Opts),
  print_conclusion(C, Opts).



%! print_expression0(+Expression:compound, +Options:list(compound))// is det.

print_expression0(error, _) --> !,
  falsum.
print_expression0(rdf(S,P,O), Opts) --> !,
  rdf_print_statement(S, P, O, _, Opts).
print_expression0(T, _) -->
  pl_term(T).



%! print_premise(
%!   +Index:positive_integer,
%!   +Premise:compound,
%!   +Options:list(compound)
%! )// is det.

print_premise(N, T, Opts) -->
  "P",
  integer(N),
  ": ",
  print_expression0(T, Opts).



%! print_premises(+Premises:list(compound), +Options:list(compound))// is det.

print_premises(Ps, Opts) -->
  print_premises(1, Ps, Opts).


%! print_premises(
%!   +Index:positive_integer,
%!   +Premises:list(compound),
%!   +Options:list(compound)
%! )// is det.

print_premises(_, [], _) --> "", !.
print_premises(N1, [H|T], Opts) -->
  "  ",
  print_premise(N1, H, Opts),
  nl,
  {succ(N1, N2)},
  print_premises(N2, T, Opts).



%! print_rule(+Rule:compound)// is det.

print_rule(R) -->
  {unwind_compound(R, L)},
  dcg_once(*(atom, L, [separator(colon)])).

unwind_compound(H, [H]):-
  atomic(H), !.
unwind_compound(H0, [H|T]):-
  H0 =.. [H,T0],
  unwind_compound(T0, T).
