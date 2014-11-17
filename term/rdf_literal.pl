:- module(
  rdf_literal,
  [
    rdf_is_langstring/1, % @Term
    rdf_langstring_data/3, % ?Field:oneof([datatype,langtag,lexical_form,value])
                           % +Literal:compound
                           % ?Data
    rdf_langstring_term/1, % ?Literal:compound
    rdf_literal_data/3, % ?Field:oneof([datatype,langtag,lexical_form,value])
                        % +Literal:compound
                        % ?Data
    rdf_literal_equiv/2, % +Literal1:compound
                         % +Literal2:compound
    rdf_literal_term/1 % ?Literal:compound
  ]
).

/** <module> RDF literal

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(term/rdf_datatype)).

:- rdf_meta(rdf_langstring_data(?,o,-)).
:- rdf_meta(rdf_langstring_term(o)).
:- rdf_meta(rdf_literal_data(+,o,-)).
:- rdf_meta(rdf_literal_equiv(r,o,o)).
:- rdf_meta(rdf_literal_term(o)).
:- rdf_meta(rdf_lexical_map(?,r,?,?)).



%! rdf_is_langstring(@Term) is semidet.

rdf_is_langstring(literal(lang(LangTag,LexicalForm))):-
  atom(LangTag),
  atom(LexicalForm).



%! rdf_langstring_data(
%!   +Field:oneof([datatype,langtag,lexical_form,value])
%!   +Literal:compound
%!   -Data
%! ) is det.
%! rdf_langstring_data(
%!   -Field:oneof([datatype,langtag,lexical_form,value])
%!   +Literal:compound
%!   -Data
%! ) is multi.

rdf_langstring_data(datatype, literal(lang(_,_)), rdf:langString).
rdf_langstring_data(langtag, literal(lang(LangTag,_)), LangTag).
rdf_langstring_data(lexical_form, literal(lang(_,LexicalForm)), LexicalForm).
rdf_langstring_data(value, literal(lang(LangTag,LexicalForm)), Value):-
  Value = LexicalForm-LangTag.



%! rdf_langstring_term(+LangTaggedString:compound) is semidet.
%! rdf_langstring_term(-LangTaggedString:compound) is nondet.
% Language tagged strings, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_langstring_term(literal(lang(LangTag,LexicalForm))):-
  rdf_current_literal(literal(lang(LangTag,LexicalForm))).



%! rdf_literal_data(
%!   +Field:oneof([datatype,langtag,lexical_form,value])
%!   +Literal:compound
%!   -Data
%! ) is det.
%! rdf_literal_data(
%!   -Field:oneof([datatype,langtag,lexical_form,value])
%!   +Literal:compound
%!   -Data
%! ) is multi.

rdf_literal_data(datatype, literal(type(Datatype,_)), Datatype).
rdf_literal_data(langtag, literal(lang(LangTag,_)), LangTag).
rdf_literal_data(lexical_form, Literal, LexicalForm):-
  (   Literal = literal(lang(_,LexicalForm))
  ->  true
  ;   Literal = literal(type(_,LexicalForm))
  ->  true
  ;   Literal = literal(LexicalForm)
  ).
rdf_literal_data(value, Literal, Value):-
  (   Literal = literal(lang(LangTag,LexicalForm))
  ->  Value = LexicalForm-LangTag
  ;   Literal = literal(type(Datatype,LexicalForm))
  ->  rdf_lexical_map(Datatype, LexicalForm, Value)
  ;   Literal = literal(LexicalForm)
  ->  Value = LexicalForm
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

% Plain literals with the same language tag and value string.
rdf_literal_equiv(
  literal(lang(LangTag,LexicalForm)),
  literal(lang(LangTag,LexicalForm))
):- !.
% Typed literals with the same Datatype and equivalent values
%  in the datatype's value space.
rdf_literal_equiv(
  literal(type(Datatype,LexicalForm1)),
  literal(type(Datatype,LexicalForm2))
):- !,
  rdf_lexical_map(Datatype, LexicalForm1, Value1),
  rdf_lexical_map(Datatype, LexicalForm2, Value2),
  rdf_equiv(Datatype, Value1, Value2).
% Simple literals that are the same.
rdf_literal_equiv(literal(LexicalForm), literal(LexicalForm)).



%! rdf_literal_term(+Literal:compound) is semidet.
%! rdf_literal_term(-Literal:compound) is nondet.

rdf_literal_term(Literal):-
  % Enumerates all literals.
  rdf_current_literal(Literal).
