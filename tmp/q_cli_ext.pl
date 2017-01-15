:- module(
  q_cli_ext,
  [
    lm/0,
    lm/1, % +HashG
    lm/3, % ?S, ?P, ?O
    lm/4, % ?S, ?P, ?O, +HashG
    lw/0,
    lw/1, % +HashG
    lw/3, % ?S, ?P, ?O
    lw/4  % ?S, ?P, ?O, +HashG
  ]
).

/** <module> LOD Laundromat CLI: CLI

@author Wouter Beek
@version 2016/03-2016/05, 2016/08
*/

:- rdf_meta
   lm(r, r, o),
   lm(r, r, o, +),
   lw(r, r, o),
   lw(r, r, o, +).





%! lm is det.
%! lm(+HashG) is det.
%! lm(?S, ?P, ?O) is det.
%! lm(?S, ?P, ?O, +HashG) is det.

lm :-
  lm('').


lm(HashG) :-
  q_cli:q__x(HashG, meta).


lm(S, P, O) :-
  lm(S, P, O, '').


lm(S, P, O, HashG) :-
  q_cli:q__x(S, P, O, HashG, meta).



%! lw is det.
%! lw(+HashG) is det.
%! lw(?S, ?P, ?O) is det.
%! lw(?S, ?P, ?O, +HashG) is det.

lw :-
  lw('').


lw(HashG) :-
  q_cli:q__x(HashG, warn).


lw(S, P, O) :-
  lw(S, P, O, '').


lw(S, P, O, HashG) :-
  q_cli:q__x(S, P, O, HashG, warn).
