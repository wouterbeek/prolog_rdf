:- module(
  rdf_api,
  [
    literal/4 % ?Literal, ?D, ?LTag, ?Lex
  ]
).
:- reexport(library(semweb/rdf11)).

/** <module> RDF API

@author Wouter Beek
@version 2017/09
*/

:- use_module(library(semweb/rdf_prefix), []).

:- rdf_meta
   literal(o, r, ?, ?).





%! literal(+Literal:compound, -D:atom, -LTag:atom, -Lex:atom) is det.
%! literal(-Literal:compound, +D:atom, +LTag:atom, +Lex:atom) is det.

literal(literal(lang(LTag,Lex)), rdf:langString, LTag, Lex) :-
  atom(LTag).
literal(literal(type(D,Lex)), D, _, Lex) :-
  atom(D).
literal(literal(Lex), _, _, Lex) :-
  atom(Lex).
