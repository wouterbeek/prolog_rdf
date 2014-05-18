:- module(
  rdf_literal,
  [
    rdf_literal/1, % ?Literal:compound
    rdf_literal/2, % ?Literal:compound
                   % ?RdfGraph:atom
    rdf_literal/3, % ?Literal:compound
                   % ?LexicalForm:atom
                   % ?DatatypeIri:iri
    rdf_literal/4, % ?Literal:compound
                   % ?LexicalForm:atom
                   % ?DatatypeIri:iri
                   % ?LanguageTag:atom
    rdf_literal/5, % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?LexicalForm:atom
                   % ?DatatypeIri:iri
                   % ?RdfGraph:atom
    rdf_literal/6, % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?LexicalForm:atom
                   % ?DatatypeIri:iri
                   % ?LanguageTag:atom
                   % ?RdfGraph:atom
    rdf_literal_equality/2, % +Literal1:compound
                            % +Literal2:compound
    rdf_literal_map/4 % ?LexicalForm:atom
                      % ?DatatypeIri:iri
                      % ?LanguageTag:atom
                      % ?Value
  ]
).

/** <module> RDF literal

Support for reading triples with literal object terms.

@author Wouter Beek
@version 2013/10, 2014/03-2014/04
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_term)).
:- use_module(xsd(xsd)).
:- use_module(xsd(xsd_clean)).

:- rdf_meta(rdf_literal(o)).
:- rdf_meta(rdf_literal(o,?)).
:- rdf_meta(rdf_literal(o,?,r)).
:- rdf_meta(rdf_literal(o,?,r,?)).
:- rdf_meta(rdf_literal(r,r,?,r,?)).
:- rdf_meta(rdf_literal(r,r,?,r,?,?)).
:- rdf_meta(rdf_literal_map(?,r,?,?)).



%! rdf_literal(+Literal:compound) is semidet.
%! rdf_literal(-Literal:compound) is nondet.

rdf_literal(Literal):-
  % Enumerates all literals.
  rdf_current_literal(Literal).


%! rdf_literal(+Literal:compound, +RdfGraph:atom) is semidet.
%! rdf_literal(+Literal:compound, -RdfGraph:atom) is nondet.
%! rdf_literal(-Literal:compound, +RdfGraph:atom) is nondet.
%! rdf_literal(-Literal:compound, -RdfGraph:atom) is nondet.

rdf_literal(Literal, G):-
  % Enumerated all literals.
  rdf_literal(Literal),
  % Relates to an RDF graph.
  rdf_object(Literal, G).


%! rdf_literal(+Literal:compound, +LexicalForm:atom, +DatatypeIri:iri) is semidet.
%! rdf_literal(+Literal:compound, -LexicalForm:atom, -DatatypeIri:iri) is det.
%! rdf_literal(-Literal:compound, +LexicalForm:atom, +DatatypeIri:iri) is det.
% Does not work for datatype `rdf:langTag`.

rdf_literal(Literal, LexicalForm, Datatype):-
  rdf_literal(Literal, LexicalForm, Datatype, _),
  \+ rdf_equal(rdf:langTag, Datatype).


%! rdf_literal(+Literal:compound, +LexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom) is semidet.
%! rdf_literal(+Literal:compound, -LexicalForm:atom, -DatatypeIri:iri, ?LanguageTag:atom) is det.
%! rdf_literal(-Literal:compound, +LexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom) is det.
% Construct/disassemble an RDF literal compound term in the Semweb format.

rdf_literal(literal(lang(LangTag,LexicalForm)), LexicalForm, rdf:langString, LangTag):- !.
rdf_literal(literal(type(Datatype,LexicalForm)), LexicalForm, Datatype, _):- !.
rdf_literal(literal(LexicalForm), LexicalForm, xsd:string, _):-
  atom(LexicalForm), !.

%! rdf_literal(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   ?LanguageTag:atom,
%!   ?RdfGraph:graph
%! ) is nondet.
% This does not work for language-tagged strings.

rdf_literal(S, P, LexicalForm, Datatype, G):-
  rdf_literal(S, P, LexicalForm, Datatype, _, G),
  \+ rdf_equal(rdf:langString, Datatype).

%! rdf_literal(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   ?LanguageTag:atom,
%!   ?RdfGraph:graph
%! ) is nondet.

% Language-tagged strings.
rdf_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  rdf_equal(rdf:langString, Datatype),
  rdf(S, P, literal(lang(LangTag,LexicalForm)), G).
% Typed literals.
rdf_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  var(LangTag),
  nonvar(Datatype),
  rdf(S, P, literal(type(Datatype,LexicalForm)), G).
% Simple literals.
rdf_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  var(LangTag),
  rdf_equal(xsd:string, Datatype),
  rdf(S, P, literal(LexicalForm), G),
  atom(LexicalForm).


%! rdf_literal_equality(+Literal1:compound, +Literal2:compound) is semidet.
% Succeeds if the given literals are equivalent.
%
% Two literals are equivalent if:
%   1. The strings of the two lexical forms compare equal,
%      character by character.
%   2. Either both or neither have language tags.
%   3. The language tags, if any, compare equal.
%   4. Either both or neither have datatype URIs.
%   5. The two datatype URIs, if any, compare equal, character by character.
%
% @see Resource Description Framework (RDF): Concepts and Abstract Syntax
%      http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/

% Plain literals with the same language tag and value string.
rdf_literal_equality(
  literal(lang(LangTag1,LexicalForm1)),
  literal(lang(LangTag2,LexicalForm2))
):- !,
  LangTag1 == LangTag2,
  LexicalForm1 == LexicalForm2.
% Typed literals with equivalent values in the datatype's value space.
rdf_literal_equality(
  literal(type(Datatype1,LexicalForm1)),
  literal(type(Datatype2,LexicalForm2))
):- !,
  Datatype1 == Datatype2,
  xsd_lexical_canonical_map(Datatype1, LexicalForm1, CanonicalLexicalForm1),
  xsd_lexical_canonical_map(Datatype2, LexicalForm2, CanonicalLexicalForm2),
  CanonicalLexicalForm1 == CanonicalLexicalForm2.
% Simple literals that are the same.
rdf_literal_equality(literal(LexicalForm1), literal(LexicalForm2)):-
  atom(LexicalForm1),
  atom(LexicalForm2),
  LexicalForm1 == LexicalForm2.


%! rdf_literal_map(-CanonicalLexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom, +Value) is det.
%! rdf_literal_map(+LexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom, -Value) is det.

% Support for =|rdf:langString|=.
rdf_literal_map(LexicalForm, rdf:langString, LangTag, lang(LangTag,LexicalForm)):- !,
  nonvar(LexicalForm),
  nonvar(LangTag).
% Support for the XSD datatypes.
rdf_literal_map(LexicalForm, Datatype, LangTag, Value):-
  xsd_datatype(Datatype),
  var(LangTag),
  (
    nonvar(Value)
  ->
    xsd_canonical_map(Datatype, Value, LexicalForm)
  ;
    nonvar(LexicalForm)
  ->
    xsd_lexical_map(Datatype, LexicalForm, Value)
  ).

