:- module(
  rdf_literal,
  [
    rdf_literal_components/4, % +Literal:compound
                              % -Datatype:iri
                              % -Language:list(atom)
                              % -Lexical:atom
    rdf_literal_data/3 % ?Field:atom
                       % +Literal:compound
                       % ?Data
  ]
).

/** <module> RDF literal

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(error)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_literal_components(o,-,-,-)).





%! rdf_literal_components(
%!   +Literal:compound,
%!   -Datatype:iri,
%!   -Language:list(atom),
%!   -Lexical:atom
%! ) is det.

rdf_literal_components(literal(type(D,Lex)), D, _, Lex):- !.
rdf_literal_components(literal(lang(Lang,Lex)), _, Lang, Lex):- !.
rdf_literal_components(literal(Lex), D, _, Lex):-
  rdf_global_id(xsd:string, D).



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

rdf_literal_data(Field, Literal, Data):-
  must_be(oneof([datatype,langtag,lexical_form,value]), Field),
  rdf_literal_data0(Field, Literal, Data).

rdf_literal_data0(datatype, literal(type(Datatype,_)), Datatype):- !.
rdf_literal_data0(datatype, literal(lang(_,_)), rdf:langString):- !.
rdf_literal_data0(datatype, literal(LexicalForm), xsd:string):-
  atom(LexicalForm).
rdf_literal_data0(langtag, literal(lang(LangTag0,_)), LangTag):-
  atomic_list_concat(LangTag, '-', LangTag0).
rdf_literal_data0(lexical_form, Literal, LexicalForm):-
  (   Literal = literal(lang(_,LexicalForm))
  ->  true
  ;   Literal = literal(type(_,LexicalForm))
  ->  true
  ;   Literal = literal(LexicalForm)
  ).
rdf_literal_data0(value, Literal, Value):-
  (   Literal = literal(lang(LangTag,LexicalForm))
  ->  Value = LexicalForm-LangTag
  ;   Literal = literal(type(Datatype,LexicalForm))
  ->  rdf_lexical_map(Datatype, LexicalForm, Value)
  ;   Literal = literal(LexicalForm)
  ->  Value = LexicalForm
  ).
