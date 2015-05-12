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
@version 2015/03
*/

:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plc(dcg/dcg_abnf_common)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(math/radix)).

:- use_module(plRdf(syntax/sw_char)).





%! atom_to_pn_local(+Atom:atom, -PnLocal:atom) is det.

atom_to_pn_local(Atom, PnLocal):-
  once(atom_phrase(to_pn_local(Codes), Atom)),
  atom_codes(PnLocal, Codes).

to_pn_local(L) -->
  to_pn_local_first(L1),
  to_pn_local_middles(L2),
  to_pn_local_last(L3), !,
  {append([L1,L2,L3], L)}.

to_pn_local_first([Code]) -->
  'PN_CHARS_U'(turtle, Code), !.
to_pn_local_first([Code]) -->
  colon(Code), !.
to_pn_local_first([Code]) -->
  '[0-9]'(_, Code), !.
to_pn_local_first([Code]) -->
  'PLX'(Code), !.
to_pn_local_first(Codes) -->
  to_pn_local_escape(Codes).

to_pn_local_middles(L) -->
  to_pn_local_middle(L1),
  to_pn_local_middles(L2),
  {append(L1, L2, L)}.
to_pn_local_middles([]) --> [].

to_pn_local_middle([Code]) -->
  'PN_CHARS'(turtle, Code), !.
to_pn_local_middle([Code]) -->
  dot(Code), !.
to_pn_local_middle([Code]) -->
  colon(Code), !.
to_pn_local_middle([Code]) -->
  'PLX'(Code), !.
to_pn_local_middle(Codes) -->
  to_pn_local_escape(Codes).

to_pn_local_last([Code]) -->
  'PN_CHARS'(turtle, Code), !.
to_pn_local_last([Code]) -->
  colon(Code), !.
to_pn_local_last([Code]) -->
  'PLX'(Code), !.
to_pn_local_last(Codes) -->
  to_pn_local_escape(Codes).

% Escape by backslash.
to_pn_local_escape([92,Code]) -->
  sw_char:'PN_LOCAL_ESC_char'(Code), !.
% Escape by hexadecimal.
to_pn_local_escape([37,Code1,Code2]) -->
  [Code],
  {
    radix(dec(Code), hex(Hex)),
    atom_chars(Hex, [Char1,Char2]),
    maplist(char_code, [Char1,Char2], [Code1,Code2])
  }.

