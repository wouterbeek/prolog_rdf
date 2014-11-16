:- module(
  rdf_literal,
  [
    rdf_literal/5, % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?LexicalForm:atom
                   % ?Datatype:iri
                   % ?RdfGraph:atom
    rdf_literal_equality/2, % +Literal1:compound
                            % +Literal2:compound
    rdf_literal_map/4 % ?LexicalForm:atom
                      % ?Datatype:iri
                      % ?LanguageTag:atom
                      % ?Value
  ]
).

/** <module> RDF literal

Support for reading triples with literal object terms.

@author Wouter Beek
@version 2013/10, 2014/03-2014/04, 2014/09
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plXsd(xsd)).
:- use_module(plXsd(xsd_clean)).

:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_literal_map(?,r,?,?)).



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


%! rdf_literal_map(-CanonicalLexicalForm:atom, +Datatype:iri, ?LanguageTag:atom, +Value) is det.
%! rdf_literal_map(+LexicalForm:atom, +Datatype:iri, ?LanguageTag:atom, -Value) is det.

% Support for =|rdf:langString|=.
rdf_literal_map(LexicalForm, rdf:langString, LangTag, lang(LangTag,LexicalForm)):- !,
  nonvar(LexicalForm),
  nonvar(LangTag).
% Support for the XSD datatypes.
rdf_literal_map(LexicalForm, Datatype, LangTag, Value):-
  xsd_datatype(Datatype),
  var(LangTag),
  (   nonvar(Value)
  ->  xsd_canonical_map(Datatype, Value, LexicalForm)
  ;   nonvar(LexicalForm)
  ->  once(xsd_lexical_map(Datatype, LexicalForm, Value))
  ).
