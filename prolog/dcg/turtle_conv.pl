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
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/sw_char)).
:- use_module(library(dlist)).
:- use_module(library(math/radconv)).





%! to_pn_local(+Atom:atom, -PnLocal:atom) is det.

to_pn_local(A, PnLocal):-
  atom_phrase(to_pn_local(Cs), A),
  atom_codes(PnLocal, Cs).

%! to_pn_local(-Codes:list(code))// .

to_pn_local(L) -->
  to_pn_local_first(L1),
  'to_pn_local_middle*'(L2),
  to_pn_local_last(L3),
  {dappend([L1,L2,L3], L)}.

%! to_pn_local_first(-Codes:dlist)// .

to_pn_local_first([C|H]-H) --> 'PN_CHARS_U'(turtle, C), !.
to_pn_local_first([C|H]-H) --> colon(C), !.
to_pn_local_first([C|H]-H) --> decimal_digit(_, C), !.
to_pn_local_first([C|H]-H) --> 'PLX'(C), !.
to_pn_local_first(L) --> to_pn_local_escape(L).

%! 'to_pn_local_middle*'(-Codes:dlist)// .

'to_pn_local_middle*'(L) -->
  to_pn_local_middle(L1),
  'to_pn_local_middle*'(L2),
  {dappend(L1, L2, L)}.
'to_pn_local_middle*'(H-H) --> [].

%! to_pn_local_middle(-Codes:list(code))// .

to_pn_local_middle([C|H]-H) --> 'PN_CHARS'(turtle, C), !.
to_pn_local_middle([C|H]-H) --> dot(C), !.
to_pn_local_middle([C|H]-H) --> colon(C), !.
to_pn_local_middle([C|H]-H) --> 'PLX'(C), !.
to_pn_local_middle(L) --> to_pn_local_escape(L).

%! to_pn_local_last(-Codes:dlist)// .

to_pn_local_last([C|H]-H) --> 'PN_CHARS'(turtle, C), !.
to_pn_local_last([C|H]-H) --> colon(C), !.
to_pn_local_last([C|H]-H) --> 'PLX'(C), !.
to_pn_local_last(L) --> to_pn_local_escape(L).

%! to_pn_local_escape(-Codes:dlist)// .

% Escape by backslash.
to_pn_local_escape([92,C|H]-H) --> sw_char:'PN_LOCAL_ESC_char'(C), !.
% Escape by hexadecimal.
to_pn_local_escape([37,C1,C2|H]-H) -->
  [C],
  {
    radconv(dec(C), hex(Hex)),
    atom_chars(Hex, [Char1,Char2]),
    maplist(char_code, [Char1,Char2], [C1,C2])
  }.
