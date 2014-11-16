:- module(
  rdf_langstring,
  [
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

:- use_module(plRdf(term/rdf_literal)).
:- use_module(plRdf(term/rdf_literal_build)).
:- use_module(plRdf(term/rdf_simple_literal)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_preferred_language_tagged_string(+,r,r,-,-,?)).



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

% Allow a single language tag to be given as argument.
rdf_preferred_language_tagged_string(LangTag1, S, P, LexicalForm, LangTag2, G):-
  \+ is_list(LangTag1), !,
  rdf_preferred_language_tagged_string([LangTag1], S, P, LexicalForm, LangTag2, G).
% A label with a preferred language tag exists.
rdf_preferred_language_tagged_string(LangTags, S, P, LexicalForm, LangTag, G):-
  % Backtracking over membership ensures
  % that we try all given language tags.
  member(LangTag, LangTags),
  rdf_langstring(S, P, LexicalForm, LangTag, G), !.
% If the given language tag cannot be matched, we take an arbitrary literal.
rdf_preferred_language_tagged_string(_, S, P, LexicalForm, LangTag, G):-
  rdf_langstring(S, P, LexicalForm, LangTag, G), !.
% If no language tagged string can be found, we look for a simple literal.
rdf_preferred_language_tagged_string(_, S, P, LexicalForm, _, G):-
  rdf_simple_literal(S, P, LexicalForm, G).

