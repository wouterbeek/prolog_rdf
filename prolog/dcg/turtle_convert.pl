:- module(
  turtle_convert,
  [
    atom_to_pn_local/2 % +Atom:atom
                       % -PnLocal:atom
  ]
).

/** <module> Turtle Convert

Predicates for converting Prolog to Turtle values.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/sw_char)).
:- use_module(library(lists)).
:- use_module(library(math/radconv)).





%! atom_to_pn_local(+Atom:atom, -PnLocal:atom) is det.

atom_to_pn_local(A, PnLocal):-
  once(atom_phrase(to_pn_local, A, PnLocal)).

%! to_pn_local// .

to_pn_local -->
  to_pn_local_first,
  'to_pn_local_middle*',
  to_pn_local_last.

%! to_pn_local_first// .

to_pn_local_first, [C] -->
  'PN_CHARS_U'(turtle, C), !.
to_pn_local_first, [C] -->
  colon(C), !.
to_pn_local_first, [C] -->
  decimal_digit(_, C), !.
to_pn_local_first, [C] -->
  'PLX'(C), !.
to_pn_local_first -->
  to_pn_local_escape.

%! 'to_pn_local_middle*'// .

'to_pn_local_middle*' --> to_pn_local_middle, 'to_pn_local_middle*'.
'to_pn_local_middle*' --> [].

%! to_pn_local_middle// .

to_pn_local_middle, [C] -->
  'PN_CHARS'(turtle, C), !.
to_pn_local_middle, [C] -->
  dot(C), !.
to_pn_local_middle, [C] -->
  colon(C), !.
to_pn_local_middle, [C] -->
  'PLX'(C), !.
to_pn_local_middle -->
  to_pn_local_escape.

%! to_pn_local_last// .

to_pn_local_last, [C] -->
  'PN_CHARS'(turtle, C), !.
to_pn_local_last, [C] -->
  colon(C), !.
to_pn_local_last, [C] -->
  'PLX'(C), !.
to_pn_local_last -->
  to_pn_local_escape.

%! to_pn_local_escape// .

% Escape by backslash.
to_pn_local_escape, [92,C] -->
  sw_char:'PN_LOCAL_ESC_char'(C), !.
% Escape by hexadecimal.
to_pn_local_escape, [37,C1,C2] -->
  [C],
  {
    radconv(dec(C), hex(Hex)),
    atom_chars(Hex, [Char1,Char2]),
    maplist(char_code, [Char1,Char2], [C1,C2])
  }.
