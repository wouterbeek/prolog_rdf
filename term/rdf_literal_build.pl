:- module(
  rdf_literal_build,
  [
    rdf_update_literal/7 % ?Subject:oneof([bnode,iri])
                         % ?Predicate:iri
                         % ?FromLexicalForm:atom
                         % ?FromDatatypeIri:iri
                         % ?FromLanguageTag:atom
                         % ?Graph:atom
                         % +Action:compound
  ]
).

/** <module> RDF literal: Build

Support for asserting/retracting triples with literal object terms.

@author Wouter Beek
@version 2013/10, 2014/03, 2014/10
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(generics(meta_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(term/rdf_literal)).

:- rdf_meta(rdf_update_literal(r,r,?,r,?,?,t)).
:- rdf_meta(rdf_update_literal_preview(r,r,?,r,?,?,+)).
:- rdf_meta(rdf_update_literal_rule(?,?,:,:,:)).



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



%! rdf_update_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?Datatype:iri,
%!   +LangTag:list(atom),
%!   +Graph:atom,
%!   +Action:compound
%! ) is det.
% Updates triples with literal objtect terms.
%
% The following actions are supported:
%   - `datatype(+Iri:atom)`
%   - `langtag(+LangTag:list(atom))`
%   - `lexical_form(+LexicalForm:atom)`
%   - `lexical_form_convert(+callable)`
%     Updates only the lexical form of matching literals algorithmically.
%     `Goal` takes the additional arguments `FromLexicalFrom`
%     and `ToLexicalForm`.
%   - `lexical_form_split(+Split:atom)`
%     Splits lexical forms, resulting into a separate triple for each split.

rdf_update_literal(Term, P, LexicalForm, Datatype, LangTag, Graph, Action):-
  rdf_update_literal_rule(
    [S,P,LexicalForm,Datatype,LangTag,Graph,Action],
      _,
      Antecedent,
      Consequent,
      _
    )
  ),
  forall(Antecedent, Consequent).


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

