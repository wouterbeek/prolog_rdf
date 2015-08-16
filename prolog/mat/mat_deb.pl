:- module(
  mat_deb,
  [
    mat_deb/3 % +Rule:compound
              % +Premises:list(compound)
              % +Conclusion:compound
  ]
).

/** <module> Debug for materialization

Debug tools for calculating a materialization.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug)).
:- use_module(library(mat/j_db)).
:- use_module(library(mat/mat_print)).





%! mat_deb(
%!   +Rule:compound,
%!   +Premises:list(compound),
%!   +Conclusion:compound
%! ) is det.

mat_deb(R, Ps, C):-
  store_j(R, Ps, C),
  (   debugging(mat(R))
  ->  dcg_with_output_to(string(S), print_deduction(R, Ps, C)),
      debug(mat(R), '~s', S)
  ;   true
  ).
