:- module(
  rdf_literal,
  [
    rdf_is_language_tagged_string/1, % @Term
    rdf_language_tagged_string/1, % -Literal:compound
    rdf_literal_components/4, % ?Literal:compound
                              % ?DatatypeIri:atom
                              % ?LexicalExpression:atom
                              % ?LanguageTag:atom
    rdf_literal_data/3, % ?Field:atom
                        % +Literal:compound
                        % ?Data
    rdf_literal_equiv/2 % +Literal1:compound
                        % +Literal2:compound
  ]
).

/** <module> RDF literal

@author Wouter Beek
@version 2015/08-2015/09, 2015/11-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(rdf/rdf_api)).

:- rdf_meta(rdf_is_language_tagged_string(o)).
:- rdf_meta(rdf_literal_components(o,r,-,-)).
:- rdf_meta(rdf_literal_equiv(o,o)).





%! rdf_is_language_tagged_string(@Term) is semidet.
% Succeeds on language-tagged strings.
%
% The **language-tagged string**s are the cartesian product of the Unicode
% strings in Normal Form C with the set of BCP 47 language tags.

rdf_is_language_tagged_string(Lit):-
  Lit = literal(lang(LTag,Lex)),
  maplist(atom, [LTag,Lex]).



%! rdf_language_tagged_string(-Literal:compound) is nondet.

rdf_language_tagged_string(Lit):-
  rdf_literal(Lit),
  rdf_is_language_tagged_string(Lit).



%! rdf_literal_components(
%!   +Literal:compound,
%!   -DatatypeIri:atom,
%!   -LexicalExpression:atom,
%!   -LanguageTag:atom
%! ) is det.
%! rdf_literal_components(
%!   -Literal:compound,
%!   +DatatypeIri:atom,
%!   +LexicalExpression:atom,
%!   +LanguageTag:atom
%! ) is det.
%! rdf_literal_components(
%!   +Literal:compound,
%!   +DatatypeIri:atom,
%!   +LexicalExpression:atom,
%!   -LanguageTag:atom
%! ) is det.
%! rdf_literal_components(
%!   +Literal:compound,
%!   -DatatypeIri:atom,
%!   +LexicalExpression:atom,
%!   -LanguageTag:atom
%! ) is det.

rdf_literal_components(Lit, D, Lex, LTag):-
  ground(Lit), !,
  rdf_literal_components0(Lit, D, Lex, LTag).
rdf_literal_components(Lit, D, Lex, LTag):-
  rdf_expand_ct(rdf:langString, D),
  atom(LTag), !,
  Lit = literal(lang(LTag,Lex)).
rdf_literal_components(Lit, D0, Lex, _):-
  ground(Lex), !,
  (ground(D0) -> D = D0 ; rdf_expand_ct(xsd:string, D)),
  Lit = literal(type(D,Lex)).
rdf_literal_components(Lit, D, Lex, LTag):-
  instantiation_error(rdf_literal_components(Lit, D, Lex, LTag)).

rdf_literal_components0(literal(type(D,Lex)), D, Lex, _):- !.
rdf_literal_components0(literal(lang(LTag,Lex)), D, Lex, LTag):- !,
  rdf_expand_ct(rdf:langString, D).
rdf_literal_components0(literal(Lex), D, Lex, _):-
  rdf_expand_ct(xsd:string, D).



%! rdf_literal_data(+Field:atom, +Literal:compound, +Data) is semidet.
%! rdf_literal_data(+Field:atom, +Literal:compound, -Data) is det.
%! rdf_literal_data(-Field:atom, +Literal:compound, -Data) is multi.
% Decomposes literals.
%
% Field is one of:
%   - `datatype`
%   - `langtag`
%   - `lexical_form`
%   - `value`
%
% @throws domain_error
% @throws type_error

rdf_literal_data(Field, Lit, Data):-
  must_be(oneof([datatype,langtag,lexical_form,value]), Field),
  rdf_literal_data0(Field, Lit, Data).

rdf_literal_data0(datatype, literal(type(D,_)), D):- !.
rdf_literal_data0(datatype, literal(lang(_,_)), D):- !,
  rdf_expand_ct(rdf:langString, D).
rdf_literal_data0(datatype, literal(Lex), D):-
  atom(Lex), !,
  rdf_expand_ct(xsd:string, D).
rdf_literal_data0(langtag, literal(lang(LTag,_)), LTag):- !.
rdf_literal_data0(lexical_form, Lit, Lex):- !,
  (   Lit = literal(lang(_,Lex))
  ->  true
  ;   Lit = literal(type(_,Lex))
  ->  true
  ;   Lit = literal(Lex)
  ).
rdf_literal_data0(value, Lit, Val):- !,
  (   Lit = literal(type(_,_))
  ->  rdf_lexical_map(Lit, Val)
  ;   Lit = literal(lang(LTag,Lex))
  ->  Val = Lex-LTag
  ;   Lit = literal(Lex)
  ->  rdf_literal_data0(value, literal(type(xsd:string,Lex)), Val)
  ).



%! rdf_literal_equiv(+Literal1:compound, +Literal2:compound) is semidet.
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
% @compat [RDF 1.0 Concepts and Abstract Syntax](http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/)
% @tbd Update to RDF 1.1.

% Equivalent language-tagged strings.
rdf_literal_equiv(literal(lang(LTag1,Lex)), literal(lang(LTag2,Lex))):- !,
  downcase_atom(LTag1, LTag0),
  downcase_atom(LTag2, LTag0).
% Equivalent typed literals have the same datatype and
% have equivalent values in the datatype's value space.
rdf_literal_equiv(Lit1, Lit2):-
  Lit1 = literal(type(D,_)),
  Lit2 = literal(type(D,_)), !,
  rdf_lexical_map(Lit1, Val1),
  rdf_lexical_map(Lit2, Val2),
  rdf_equiv_value(D, Val1, Val2).
% Simple literal on left hand side.
rdf_literal_equiv(literal(Lex), Lit2):- !,
  rdf_expand_ct(xsd:string, D),
  rdf_literal_equiv(literal(type(D,Lex)), Lit2).
% Simple literal on right hand side.
rdf_literal_equiv(Lit1, literal(Lex)):- !,
  rdf_expand_ct(xsd:string, D),
  rdf_literal_equiv(Lit1, literal(type(D,Lex))).
