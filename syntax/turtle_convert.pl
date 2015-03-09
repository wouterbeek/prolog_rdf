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

:- use_module(plc(dcg/dcg_abnf_common)).
:- use_module(plc(dcg/dcg_generics)).

:- use_module(plRdf(syntax/sw_char)).





%! atom_to_pn_local(+Atom:atom, -PnLocal:atom) is det.

atom_to_pn_local(Atom, PnLocal):-
  dcg_phrase(to_pn_local, Atom, PnLocal).

to_pn_local -->
  to_pn_local_first,
  to_pn_local_middles,
  to_pn_local_last.

to_pn_local_first -->
  to_pn_local_common, !.
to_pn_local_first, [Code] -->
  '[0-9]'(_, Code).

to_pn_local_middles -->
  to_pn_local_middle, !,
  to_pn_local_middles.
to_pn_local_middles --> [].

to_pn_local_middle -->
  to_pn_local_common, !.
to_pn_local_middle, "." -->
  ".".

to_pn_local_last -->
  to_pn_local_common.

to_pn_local_common, [Code] -->
  'PN_CHARS_U'(turtle, Code), !.
to_pn_local_common, ":" -->
  ":", !.
to_pn_local_common, [Code] -->
  'PLX'(Code).

