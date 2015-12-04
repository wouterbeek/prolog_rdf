:- module(
  rdf_legacy,
  [
    rdf_is_plain_literal/1, % @Term
    rdf_is_simple_literal/1, % @Term
    rdf_is_typed_literal/1, % @Term
    rdf_plain_literal/1, % -Literal:compound
    rdf_simple_literal/1, % -Literal:compound
    rdf_typed_literal/1 % -Literal:compound
  ]
).

/** <module> RDF Legacy

# RDF 1.0 Literals

Support for RDF 1.0 simple, plain, and typed literals.
Simple, plain, and typed literals are obsolete in RDF 1.1.

The **simple literals** used to be defined as the Unicode strings
in Normal Form C.

The **plain literals** used to be defined as the union of the Unicode strings
in Normal Form C and the cartesian product of the Unicode strings
in Normal Form C with the set of BCP 47 language tags.

The **typed literals** used to be defined as the cartesian product of
the Unicode strings in Normal Form C with the set of datatype URIs.

@author Wouter Beek
@version 2015/09-2015/10, 2015/12
*/

:- use_module(library(rdf/rdf_api)).
:- use_module(library(typecheck)).

:- rdf_meta(rdf_is_typed_literal(o)).





%! rdf_is_plain_literal(@Term) is semidet.
% Succeeds if the given Term denotes a plain literal.

rdf_is_plain_literal(T):-
  rdf_is_simple_literal(T).
rdf_is_plain_literal(T):-
  rdf_is_language_tagged_string(T).



%! rdf_is_simple_literal(@Term) is semidet.
% Succeeds if the given Term denotes a plain literal.

rdf_is_simple_literal(T):-
  T = literal(Lex),
  atom(Lex).



%! rdf_is_typed_literal(@Term) is semidet.
% Succeeds if the given Term denotes a typed literal.

rdf_is_typed_literal(Lit):-
  Lit = literal(type(D,Lex)),
  is_iri(D),
  atom(Lex).



%! rdf_plain_literal(-Literal:compound) is nondet.

rdf_plain_literal(Lit):-
  rdf_literal(Lit),
  rdf_is_plain_literal(Lit).



%! rdf_simple_literal(-Literal:compound) is nondet.

rdf_simple_literal(Lit):-
  rdf_literal(Lit),
  rdf_is_simple_literal(Lit).



%! rdf_typed_literal(-Literal:compound) is nondet.

rdf_typed_literal(Lit):-
  rdf_literal(Lit),
  rdf_is_typed_literal(Lit).
