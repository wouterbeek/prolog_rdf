:- module(
  turtle_conv,
  [
    to_pn_local/2 % +Atom:atom
                  % -PnLocal:atom
  ]
).

/** <module> Turtle Convert

Predicates for converting Prolog to Turtle values.

@author Wouter Beek
@version 2015/08, 2015/11, 2016/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/turtle11), []).
:- use_module(library(dcg/sparql10), ['PN_CHARS'//1,'PN_CHARS_U'//1]).
:- use_module(library(dcg/sparql11), ['PLX'//1]).
:- use_module(library(dlist)).
:- use_module(library(math/radconv)).





%! to_pn_local(+Atom:atom, -PnLocal:atom) is det.

to_pn_local(A1, A2) :-
  atom_phrase(to_pn_local(A2), A1).

to_pn_local(A) -->
  to_pn_local_first(L1),
  to_pn_local_middle1(L2),
  to_pn_local_last(L3),
  {
    dappend([L1,L2,L3], L),
    atom_codes(A, L)
  }.

to_pn_local_first([C  |H]-H) --> 'PN_CHARS_U'(C), !.
to_pn_local_first([0':|H]-H) --> ":",             !.
to_pn_local_first([C  |H]-H) --> digit(C),        !.
to_pn_local_first([C  |H]-H) --> 'PLX'(C),        !.
to_pn_local_first(L) --> to_pn_local_escape(L).

to_pn_local_middle1(L) -->
  to_pn_local_middle2(L1),
  to_pn_local_middle1(L2),
  {dappend(L1, L2, L)}.
to_pn_local_middle1(H-H) --> [].

to_pn_local_middle2([C  |H]-H) --> 'PN_CHARS'(C), !.
to_pn_local_middle2([0'.|H]-H) --> ".",           !.
to_pn_local_middle2([0':|H]-H) --> ":",           !.
to_pn_local_middle2([C  |H]-H) --> 'PLX'(C),      !.
to_pn_local_middle2(L)         --> to_pn_local_escape(L).

to_pn_local_last([C  |H]-H) --> 'PN_CHARS'(C), !.
to_pn_local_last([0':|H]-H) --> ":",           !.
to_pn_local_last([C  |H]-H) --> 'PLX'(C),      !.
to_pn_local_last(L)         --> to_pn_local_escape(L).

% Escape by backslash.
to_pn_local_escape([0'\\,C|H]-H) --> sparql11:pn_local_esc_code(C), !.
% Escape by hexadecimal.
to_pn_local_escape([0'%,Code1,Code2|H]-H) -->
  [Code],
  {
    radconv(dec(Code), hex(Hex)),
    atom_chars(Hex, [Char1,Char2]),
    maplist(char_code, [Char1,Char2], [Code1,Code2])
  }.
