:- module(
  rdf_typed_literal,
  [
    rdf_typed_literal/1, % ?TypedLiteral:compound
    rdf_typed_literal/2, % ?TypedLiteral:compound
                         % ?RdfGraph:atom
    rdf_typed_literal/3 % ?TypedLiteral:compound
                        % ?LexicalForm:atom
                        % ?DatatypeIri:iri
  ]
).

/** <module> RDF typed literal

Support for RDF 1.0 typed literals.
Typed literals are obsolete in RDF 1.1.

A typed literal used to be defined as the cartesian product of
the Unicode strings in Normal Form C with the set of datatype URIs.

@author Wouter Beek
@version 2013/09-2013/11, 2014/01, 2014/03
*/

:- use_module(generics(typecheck)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_term)).

:- rdf_meta(rdf_typed_literal(o)).
:- rdf_meta(rdf_typed_literal(o,?)).
:- rdf_meta(rdf_typed_literal(o,?,r)).



%! rdf_is_typed_literal(+RdfTerm:or([bnode,iri,literal])) is semidet.

rdf_is_typed_literal(literal(type(IRI,LexicalForm))):-
  is_of_type(iri, IRI),
  atom(LexicalForm).


%! rdf_typed_literal(+TypedLiteral:compound) is semidet.
%! rdf_typed_literal(-TypedLiteral:compound) is nondet.
% Typed literal compound terms, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_typed_literal(TypedLiteral):-
  % Enumerate all literals.
  rdf_current_literal(TypedLiteral),
  % Make sure the literal is typed.
  rdf_is_typed_literal(TypedLiteral).


%! rdf_typed_literal(+TypedLiteral:compound, +RdfGraph:atom) is semidet.
%! rdf_typed_literal(+TypedLiteral:compound, -RdfGraph:atom) is nondet.
%! rdf_typed_literal(-TypedLiteral:compound, +RdfGraph:atom) is nondet.
%! rdf_typed_literal(-TypedLiteral:compound, -RdfGraph:atom) is nondet.
% Pairs of RDF graphs to typed literals.
% Enumeration is assured to not deliver any duplicates.

rdf_typed_literal(TypedLiteral, G):-
  rdf_typed_literal(TypedLiteral),
  rdf_object(TypedLiteral, G).


%! rdf_typed_literal(+TypedLiteral:compound, +LexicalForm:atom, +DatatypeIri:iri) is semidet.
%! rdf_typed_literal(+TypedLiteral:compound, -LexicalForm:atom, -DatatypeIri:iri) is det.
%! rdf_typed_literal(-TypedLiteral:compound, +LexicalForm:atom, +DatatypeIri:iri) is det.
%! rdf_typed_literal(-TypedLiteral:compound, -LexicalForm:atom, -DatatypeIri:iri) is nondet.
%
% ### Mode enumeration
%
% Mode (-,-,-) enumerates the asserted language-tagged strings.
% The other modes compose/decompose language-tagged strings without
% them having to exist in the store.

rdf_typed_literal(TypedLiteral, LexicalForm, Datatype):-
  var(TypedLiteral),
  var(LexicalForm),
  var(Datatype), !,
  % Enumerate all typed literals.
  rdf_typed_literal(TypedLiteral),
  TypedLiteral = literal(type(Datatype,LexicalForm)).
rdf_typed_literal(literal(type(Datatype,LexicalForm)), LexicalForm, Datatype).

