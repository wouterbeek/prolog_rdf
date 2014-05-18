:- module(
  rdf_literal_build,
  [
    rdf_assert_literal/5, % +Subject:oneof([bnode,iri])
                          % +Predicate:iri
                          % +LexicalForm:atom
                          % +DatatypeIri:iri
                          % +RdfGraph:atom
    rdf_assert_literal/6, % +Subject:oneof([bnode,iri])
                          % +Predicate:iri
                          % +LexicalForm:atom
                          % +DatatypeIri:iri
                          % +LanguageTag:atom
                          % +RdfGraph:atom
    rdf_retractall_literal/5, % ?Subject:oneof([bnode,iri])
                              % ?Predicate:iri
                              % ?LexicalForm:atom
                              % ?DatatypeIri:iri
                              % ?RdfGraph:atom
    rdf_retractall_literal/6, % ?Subject:oneof([bnode,iri])
                              % ?Predicate:iri
                              % ?LexicalForm:atom
                              % ?DatatypeIri:iri
                              % ?LanguageTag:atom
                              % ?RdfGraph:atom
    rdf_update_literal/7, % ?Subject:oneof([bnode,iri])
                          % ?Predicate:iri
                          % ?FromLexicalForm:atom
                          % ?FromDatatypeIri:iri
                          % ?FromLanguageTag:atom
                          % ?RdfGraph:atom
                          % +Action:compound
    rdf_update_literal_preview/7 % ?Subject:oneof([bnode,iri])
                                 % ?Predicate:iri
                                 % ?FromLexicalForm:atom
                                 % ?FromDatatypeIri:iri
                                 % ?FromLanguageTag:atom
                                 % ?RdfGraph:atom
                                 % +Action:compound
  ]
).

/** <module> RDF literal build

Support for asserting/retracting triples with literal object terms.

@author Wouter Beek
@version 2013/10, 2014/03
*/

:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_web(rdf_store_table)).

:- rdf_meta(rdf_assert_literal(r,r,+,r,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,r,+,+)).
:- rdf_meta(rdf_retractall_literal(r,r,?,r,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,r,?,?)).
:- rdf_meta(rdf_update_literal(r,r,?,r,?,?,t)).
:- rdf_meta(rdf_update_literal_preview(r,r,?,r,?,?,+)).
:- rdf_meta(rdf_update_literal_rule(?,?,:,:,:)).



%! rdf_assert_literal(
%!   +Subject:oneof([bnode,iri]),
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   +RdfGraph:atom
%! ) is det.
% Asserts a triple with a literal object term.
%
% If the datatype IRI is omitted, the XSD string datatype is used.
%
% This cannot be used to assert triples with a literal object term
% that is of type =|rdf:langString|=

rdf_assert_literal(S, P, LexicalForm, Datatype, Graph):-
  rdf_assert_literal(S, P, LexicalForm, Datatype, _, Graph).


%! rdf_assert_literal(
%!   +Subject:oneof([bnode,iri]),
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   +LanguageTag:atom,
%!   +RdfGraph:atom
%! ) is det.
% Asserts a triple with a literal object term.

% Language-tagged strings.
rdf_assert_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  nonvar(LangTag), !,
  % The datatype IRI is =|rdf:langString|= iff the language tag is set.
  rdf_equal(rdf:langString, Datatype),
  rdf_assert(S, P, literal(lang(LangTag,LexicalForm)), G).
% Simple literals.
rdf_assert_literal(S, P, LexicalForm, Datatype, _, G):-
  var(Datatype), !,
  rdf_assert_literal(S, P, LexicalForm, xsd:string, _, G).
% Others.
rdf_assert_literal(S, P, LexicalForm, Datatype, _, G):-
  rdf_assert(S, P, literal(type(Datatype,LexicalForm)), G).


%! rdf_convert_literal(
%!   +FromLexicalForm:atom,
%!   +FromDatatypeIri:iri,
%!   ?FromLanguageTag:atom,
%!   -ToLexicalForm:atom,
%!   ?ToDatatypeIri:iri,
%!   ?ToLanguageTag:atom
%! ) is det.
% If `ToDatatype` is uninstantiated, it is equated to `FromDatatype`.

rdf_convert_literal(
  FromLexicalForm, FromDatatype, FromLangTag,
  ToLexicalForm,   ToDatatype,   ToLangTag
):-
  atom(FromLexicalForm),
  nonvar(FromDatatype),
  default(FromDatatype, ToDatatype),
  rdf_literal_map(FromLexicalForm, FromDatatype, FromLangTag, Value),
  rdf_literal_map(ToLexicalForm,   ToDatatype,   ToLangTag,   Value).


%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   ?RdfGraph:atom
%! ) is det.
% Retracts all matching RDF triples that have literal object terms.
%
% Does not retract language-tagged strings for specific language tags.

rdf_retractall_literal(S, P, LexicalForm, Datatype, G):-
  rdf_retractall_literal(S, P, LexicalForm, Datatype, _, G).


%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   ?LanguageTag:atom,
%!   ?RdfGraph:atom
%! ) is det.
% Retracts all matching RDF triples that have literal object terms.
%
% Implementation note: this assumes that simple literals are always
% asserted with datatype IRI =|xsd:string|=.
% We do not retract literal compound terms of the form
% =|literal(LexicalForm:atom)|=.

% Retract language-tagged strings.
rdf_retractall_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  (rdf_equal(rdf:langString, Datatype) ; nonvar(LangTag)), !,
  rdf_retractall(S, P, literal(lang(LangTag,LexicalForm)), G).
% Retract others.
rdf_retractall_literal(S, P, LexicalForm, Datatype, _, G):-
  rdf_retractall(S, P, literal(type(Datatype,LexicalForm)), G).


%! rdf_update_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?FromLexicalForm:atom,
%!   ?FromDatatype:iri,
%!   +FromLanguageTag:atom,
%!   +RdfGraph:atom,
%!   +Action:compound
%! ) is det.
% Updates triples with literal objtect terms.
%
% The following actions are supported:
%   * =|lexical_form(:Goal)|=
%     Updates only the lexical form of matching literals algorithmically.
%     `Goal` takes the additional arguments `FromLexicalFrom`
%     and `ToLexicalForm`.
%   * =|literal(+ToLexicalForm:atom,?ToDatatypeIri:iri,?ToLanguageTag)|=
%     Updates matching literal's components.
%   * =|split_lexical_form(+Split:atom)|=
%     Splits lexical forms, resulting into a separate triple for each split.

rdf_update_literal(S, P, FromLexicalForm, FromDatatype, FromLangTag, G, Action):-
  % Take the first matching rule.
  once(
    rdf_update_literal_rule(
      [S,P,FromLexicalForm,FromDatatype,FromLangTag,G,Action],
      _,
      Antecedent,
      Consequent,
      _
    )
  ),
  forall(Antecedent, Consequent).


%! rdf_update_literal_preview(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?FromLexicalForm:atom,
%!   ?FromDatatype:iri,
%!   +FromLanguageTag:atom,
%!   +RdfGraph:atom,
%!   +Action:compound
%! ) is det.
% Generates a preview of what would happen if
% triples with literal objtect terms would be updated.
%
% The supported actions are documented for rdf_update_literal/7.

rdf_update_literal_preview(S, P, FromLexicalForm, FromDatatype, FromLangTag,
    G, Action):-
  % Take the first matching rule.
  once(
    rdf_update_literal_rule(
      [S,P,FromLexicalForm,FromDatatype,FromLangTag,G,Action],
      Header,
      Antecedent,
      Consequent,
      Preview
    )
  ),
  findall(
    Header,
    (
      Antecedent,
      Consequent,
      Preview
    ),
    Rows
  ),
  rdf_store_rows(Rows).


%! rdf_update_literal_rule(
%!   ?Match:list,
%!   ?Header:list,
%!   :Antecedent,
%!   :Consequent,
%!   :Preview
%! ) is nondet.
% Rules that can be used by:
%   * rdf_update_literal/7
%   * rdf_update_literal_preview/7

% Replace literal components.
rdf_update_literal_rule(
  [S,P,FromLexicalForm,FromDatatype,FromLangTag,G,
      literal(ToLexicalForm,ToDatatype,ToLangTag)],
  [S,P,FromLiteral,ToLiteral,G],
  rdf_literal(S, P, FromLexicalForm, FromDatatype, FromLangTag, G),
  (
    rdf_convert_literal(
      FromLexicalForm,
      FromDatatype,
      FromLangTag,
      ToLexicalForm,
      ToDatatype,
      ToLangTag
    ),
    rdf_retractall_literal(
      S,
      P,
      FromLexicalForm,
      FromDatatype,
      FromLangTag,
      G
    ),
    rdf_assert_literal(S, P, ToLexicalForm, ToDatatype, ToLangTag, G)
  ),
  (
    rdf_literal(FromLiteral, FromLexicalForm, FromDatatype, FromLangTag),
    rdf_literal(ToLiteral,   ToLexicalForm,   ToDatatype,   ToLangTag  )
  )
).
% Transformation of lexical forms.
rdf_update_literal_rule(
  [S,P,FromLexicalForm,Datatype,LangTag,G,lexical_form(Goal)],
  [S,P,FromLiteral,ToLiteral,G],
  rdf_literal(S, P, FromLexicalForm, Datatype, LangTag, G),
  (
    call(Goal, FromLexicalForm, ToLexicalForm),

    % The lexical form must be altered.
    FromLexicalForm \== ToLexicalForm,

    % Make sure the altered lexical form still results in an valid literal.
    rdf_literal_map(ToLexicalForm, Datatype, LangTag, _),

    % Assert the new lexical form and retract the old one.
    rdf_assert_literal(    S, P, ToLexicalForm,   Datatype, LangTag, G),
    rdf_retractall_literal(S, P, FromLexicalForm, Datatype, LangTag, G)
  ),
  (
    rdf_literal(FromLiteral, FromLexicalForm, Datatype, LangTag),
    rdf_literal(ToLiteral,   ToLexicalForm,   Datatype, LangTag)
  )
).
% Splitting of lexical forms.
rdf_update_literal_rule(
  [S,P,FromLexicalForm,Datatype,LangTag,G,split_lexical_form(Split)],
  [S,P,FromLiteral,ToLiteral,G],
  rdf_literal(S, P, FromLexicalForm, Datatype, LangTag, G),
  (
    atomic_list_concat(ToLexicalForms, Split, FromLexicalForm),

    % The split must be non-trivial.
    ToLexicalForms \== [_],

    % Make sure the altered lexical forms still result in valid literals.
    forall(
      member(ToLexicalForm, ToLexicalForms),
      rdf_literal_map(ToLexicalForm, Datatype, LangTag, _)
    ),

    % Assert all the splitted lexical forms.
    forall(
      member(ToLexicalForm, ToLexicalForms),
      rdf_assert_literal(    S, P, ToLexicalForm,   Datatype, LangTag, G)
    ),
    % Retract the single non-splitted lexical form.
    rdf_retractall_literal(S, P, FromLexicalForm, Datatype, LangTag, G)
  ),
  (
    rdf_literal(FromLiteral, FromLexicalForm, Datatype, LangTag),
    rdf_literal(ToLiteral,   ToLexicalForm,   Datatype, LangTag)
  )
).

