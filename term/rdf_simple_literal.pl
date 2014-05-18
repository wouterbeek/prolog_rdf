:- module(
  rdf_simple_literal,
  [
    rdf_assert_simple_literal/4, % +Subject:or([bnode,iri,literal]),
                                 % +Predicate:iri,
                                 % +LexicalForm:atom,
                                 % +RdfGraph:atom
    rdf_simple_literal/1, % ?SimpleLiteral:compound
    rdf_simple_literal/2, % ?SimpleLiteral:compound
                          % ?RdfGraph:atom
    rdf_simple_literal/4, % ?Subject:or([bnode,iri])
                          % ?Predicate:iri
                          % ?LexicalForm:atom
                          % ?RdfGraph:atom
    rdf_simple_literal_lexical_form/2 % ?SimpleLiteral:compound
                                      % ?LexicalForm:atom
  ]
).

/** <module> RDF simple literal

Support for RDF 1.0 and RDF 1.1 simple literals.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdf_term(rdf_term)).

:- rdf_meta(rdf_assert_simple_literal(r,r,?,?)).
:- rdf_meta(rdf_simple_literal(r,r,?,?)).



%! rdf_assert_simple_literal(
%!   +Subject:or([bnode,iri,literal]),
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   +RdfGraph:atom
%! ) is det.
% Asserts a simple literal.

rdf_assert_simple_literal(Subject, Predicate, LexicalForm, Graph):-
  rdf_assert_string(Subject, Predicate, LexicalForm, Graph).


%! rdf_is_simple_literal(+RdfTerm:or([bnode,iri,literal])) is semidet.
% Succeeds if the given RDF term is a simple literal.

rdf_is_simple_literal(literal(LexicalForm)):-
  % Exclude cases in which `Lex` is a compound term,
  % i.e., either `lang(LangTag,Value)` or `type(Type,Lexical)`.
  atom(LexicalForm).


%! rdf_simple_literal(+SimpleLiteral:atom) is semidet.
%! rdf_simple_literal(-SimpleLiteral:atom) is nondet.
% Simple literal compound terms, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_simple_literal(SimpleLiteral):-
  % Enumerate all literals.
  rdf_current_literal(SimpleLiteral),
  % Make sure the literal is simple.
  rdf_is_simple_literal(SimpleLiteral).


%! rdf_simple_literal(+SimpleLiteral:compound, +RdfGraph:atom) is semidet.
%! rdf_simple_literal(+SimpleLiteral:compound, +RdfGraph:atom) is nondet.
%! rdf_simple_literal(-SimpleLiteral:compound, -RdfGraph:atom) is nondet.
%! rdf_simple_literal(-SimpleLiteral:compound, -RdfGraph:atom) is nondet.
% Relates simple literals to the RDF graph in which they occur.
% Enumeration is assured to not deliver any pair duplicates.

rdf_simple_literal(SimpleLiteral, G):-
  % Enumerate all (i.e. any graph) simple literals without duplicates.
  rdf_simple_literal(SimpleLiteral),
  % Relate the simple literal to a graph.
  rdf_object(SimpleLiteral, G).


%! rdf_simple_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?Graph:atom
%! ) is nondet.

rdf_simple_literal(S, P, LexicalForm, G):-
  rdf(S, P, literal(LexicalForm), G),
  atom(LexicalForm).


%! rdf_simple_literal_lexical_form(+SimpleLiteral:compound, +LexicalForm:atom) is semidet.
%! rdf_simple_literal_lexical_form(+SimpleLiteral:compound, -LexicalForm:atom) is det.
%! rdf_simple_literal_lexical_form(-SimpleLiteral:compound, +LexicalForm:atom) is det.
% Converts between a simple literal and its lexical form

rdf_simple_literal_lexical_form(literal(LexicalForm), LexicalForm):-
  atom(LexicalForm).

