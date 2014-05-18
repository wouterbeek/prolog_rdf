:- module(
  rdf_language_tagged_string,
  [
    rdf_assert_language_tagged_string/5, % +Subject:or([bnode,iri])
                                         % +Predicate:iri
                                         % +LexicalForm:atom
                                         % +LangTag:atom
                                         % +RdfGraph:atom
    rdf_language_tagged_string/1, % ?LanguageTaggedString:compound
    rdf_language_tagged_string/2, % ?LanguageTaggedString:compound
                                  % ?RdfGraph:atom
    rdf_language_tagged_string/3, % ?LanguageTaggedString:compound
                                  % ?LexicalForm:atom
                                  % ?LangTag:atom
    rdf_language_tagged_string/5, % ?Subject:or([bnode,iri])
                                  % ?Predicate:iri
                                  % ?LexicalForm:atom
                                  % ?LangTag:atom
                                  % ?RdfGraph:atom
    rdf_preferred_language_tagged_string/6 % +LanguageTags:or([atom,list(atom)])
                                           % ?Subject:or([bnode,iri])
                                           % ?Predicate:iri
                                           % -LexicalForm:atom
                                           % -LangTag:atom
                                           % ?Graph:atom
  ]
).

/** <module> RDF language tagged string

Support for RDF 1.1 language tagged strings.

@author Wouter Beek
@version 2013/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_literal_build)).
:- use_module(rdf_term(rdf_simple_literal)).
:- use_module(rdf_term(rdf_term)).

:- rdf_meta(rdf_assert_language_tagged_string(r,r,+,+,+)).
:- rdf_meta(rdf_language_tagged_string(o)).
:- rdf_meta(rdf_language_tagged_string(o,?)).
:- rdf_meta(rdf_language_tagged_string(o,?,?)).
:- rdf_meta(rdf_language_tagged_string(r,r,?,?,?)).
:- rdf_meta(rdf_preferred_language_tagged_string(+,r,r,-,-,?)).



%! rdf_assert_language_tagged_string(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   +LangTag:atom,
%!   +RdfGraph:atom
%! ) is det.

rdf_assert_language_tagged_string(S, P, LexicalForm, LangTag, Graph):-
  rdf_assert_literal(S, P, LexicalForm, rdf:langString, LangTag, Graph).


%! rdf_is_language_tagged_string(+RdfTerm:or([bnode,iri,literal])) is semidet.

rdf_is_language_tagged_string(literal(lang(LangTag,LexicalForm))):-
  atom(LangTag),
  atom(LexicalForm).


%! rdf_language_tagged_string(+LanguageTaggedString:compound) is semidet.
%! rdf_language_tagged_string(-LanguageTaggedString:compound) is nondet.
% Language tagged strings, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_language_tagged_string(LanguageTaggedString):-
  rdf_current_literal(LanguageTaggedString),
  rdf_is_language_tagged_string(LanguageTaggedString).


%! rdf_language_tagged_string(+LanguageTaggedString:compound, +RdfGraph:atom) is semidet.
%! rdf_language_tagged_string(+LanguageTaggedString:compound, -RdfGraph:atom) is nondet.
%! rdf_language_tagged_string(-LanguageTaggedString:compound, +RdfGraph:atom) is nondet.
%! rdf_language_tagged_string(-LanguageTaggedString:compound, -RdfGraph:atom) is nondet.
% RDF graphs and their language tagged strings, according to the Semweb format.
% Enumeration is assured to not deliver any pairs duplicates.

rdf_language_tagged_string(LanguageTaggedString, G):-
  % Enumerate language tagged strings.
  rdf_language_tagged_string(LanguageTaggedString),
  % Relate to an RDF graph.
  rdf_object(LanguageTaggedString, G).


%! rdf_language_tagged_string(+LanguageTaggedString:compound, +LexicalForm:atom, +LangTag:atom) is semidet.
%! rdf_language_tagged_string(+LanguageTaggedString:compound, -LexicalForm:atom, -LangTag:atom) is det.
%! rdf_language_tagged_string(-LanguageTaggedString:compound, +LexicalForm:atom, +LangTag:atom) is det.
%! rdf_language_tagged_string(-LanguageTaggedString:compound, -LexicalForm:atom, -LangTag:atom) is nondet.
% Relates a language-tagged string to its constituent parts:
% a lexical form and a language tag.
%
% ### Mode enumeration
%
% Mode (-,-,-) enumerates the asserted language-tagged strings.
% The other modes compose/decompose language-tagged strings without
% them having to exist in the store.

rdf_language_tagged_string(LanguageTaggedString, LexicalForm, LangTag):-
  var(LanguageTaggedString),
  var(LexicalForm),
  var(LangTag), !,
  % Enumerate all language tagged strings.
  rdf_language_tagged_string(LanguageTaggedString),
  % Extract the language tag component.
  LanguageTaggedString = literal(lang(LangTag,LexicalForm)).
rdf_language_tagged_string(literal(lang(LangTag,LexicalForm)), LexicalForm, LangTag).


%! rdf_language_tagged_string(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?LangTag:atom,
%!   ?RdfGraph:atom
%! ) is nondet.

rdf_language_tagged_string(Subject, Predicate, LexicalForm, LangTag, Graph):-
  rdf_literal(Subject, Predicate, LexicalForm, rdf:langString, LangTag, Graph).


%! rdf_preferred_language_tagged_string(
%!   +LanguageTags:or([atom,list(atom)]),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?LangTag:atom,
%!   ?RdfGraph:atom
%! ) is nondet.
% Look for the preferred languages, in order of occurrence in
% the given list of language tags.

rdf_preferred_language_tagged_string(LangTag1, S, P, LexicalForm, LangTag2, G):-
  \+ is_list(LangTag1), !,
  rdf_preferred_language_tagged_string([LangTag1], S, P, LexicalForm, LangTag2, G).
rdf_preferred_language_tagged_string(LangTags, S, P, LexicalForm, LangTag, G):-
  % Backtracking over membership ensures
  % that we try all given language tags.
  member(LangTag, LangTags),
  rdf_language_tagged_string(S, P, LexicalForm, LangTag, G), !.
% If the given language tag cannot be matched at all,
% take an arbitrary literal.
rdf_preferred_language_tagged_string(_, S, P, LexicalForm, LangTag, G):-
  rdf_language_tagged_string(S, P, LexicalForm, LangTag, G), !.
% No language tag at all.
rdf_preferred_language_tagged_string(_, S, P, LexicalForm, _, G):-
  rdf_simple_literal(S, P, LexicalForm, G).

