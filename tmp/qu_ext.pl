:- module(
  qu_ext,
  [
    qu_remove_thousands/4     % +M1, +M2, +P, +G
  ]
).

/** <module> Quine update extensions

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_atom)).
:- use_module(library(debug)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qu)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   qu_remove_thousands(+, +, r, r).





%! qu_remove_thousands(+M1, +M2, +P, G) is det.

qu_remove_thousands(M1, M2, P, G) :-
  qu_change_lex(M1, M2, P, G, remove_thousands0).


remove_thousands0 -->
  ",", !,
  remove_thousands0.
remove_thousands0, digit(D) -->
  digit(D), !,
  remove_thousands0.
remove_thousands0 --> "".
