:- module(
  rdf_datatype,
  [
    rdf_canonical_map/3, % +Datatype:iri
                         % +Value
                         % -CanonicalLiteral:compound
    rdf_canonical_map/4, % +Datatype:iri
                         % +Value
                         % -CanonicalLexicalExpression:atom
                         % -CanonicalLanguageTag:atom
    rdf_compare_value/4, % +Datatype:iri
                         % -Order:oneof([incomparable,<,=,>])
                         % +Value1
                         % +Value2
    rdf_datatype/1, % ?Datatype:iri
    rdf_datatype/2, % ?Datatype:iri
                    % ?PrologType
    rdf_equiv_value/3, % +Datatype:iri
                       % +Value1
                       % +Value2
    rdf_guess_datatype/2, % +Value
                          % -Datatype:iri
    rdf_interpreted_term/2, % +Term1:rdf_term
                            % -Term2
    rdf_lexical_canonical_map/2, % +Literal:compound
                                 % ?CanonicalLexicalFrom:atom
    rdf_lexical_canonical_map/5, % +Datatype:iri
                                 % +LexicalExpression:atom
                                 % +LanguageTag:atom
                                 % -CanonicalLexicalExpression:atom
                                 % -CanonicalLanguageTag:atom
    rdf_lexical_map/2, % +Literal:compound
                       % -Value
    rdf_lexical_map/3, % +Datatype:iri
                       % +LexicalExpression:atom
                       % -Value
    rdf_lexical_map/4, % +Datatype:iri
                       % +LexicalExpression:atom
                       % +LanguageTag:atom
                       % -Value
    rdf_subtype_of/2 % ?SubType:iri
                     % ?SuperType:iri
  ]
).

/** <module> RDF datatype

@author Wouter Beek
@compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/)
@license MIT License
@version 2015/07-2015/09, 2015/11-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug_ext)).
:- use_module(library(html/html_dom)).
:- use_module(library(ltag/rfc5646)).
:- use_module(library(memfile)).
:- use_module(library(rdf/rdf_literal)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdf/w3c_dtf)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(typecheck)).
:- use_module(library(xml/xml_dom)).
:- use_module(library(xsd/xsd)).
:- use_module(library(xsd/xsd_update)).

:- rdf_meta(rdf_canonical_map(r,+,-)).
:- rdf_meta(rdf_canonical_map(r,+,-,-)).
:- rdf_meta(rdf_compare_value(r,?,+,+)).
:- rdf_meta(rdf_datatype(r)).
:- rdf_meta(rdf_datatype(r,?)).
:- rdf_meta(rdf_datatype_term(r)).
:- rdf_meta(rdf_datatype_term(r,?)).
:- rdf_meta(rdf_equiv_value(r,+,+)).
:- rdf_meta(rdf_interpreted_term(o,-)).
:- rdf_meta(rdf_lexical_canonical_map(o,-)).
:- rdf_meta(rdf_lexical_canonical_map(r,+,+,-,-)).
:- rdf_meta(rdf_lexical_map(o,-)).
:- rdf_meta(rdf_lexical_map(r,+,-)).
:- rdf_meta(rdf_lexical_map(r,+,+,-)).
:- rdf_meta(rdf_subtype_of(r,r)).





%! rdf_canonical_map(+Datatype:iri, +Value, -CanonicalLiteral:compound) is det.
% Maps RDF datatyped values onto a unique / canonical lexical form.
%
% Supports the following RDF datatypes:
%   - `rdf:langString`
%   - `rdf:HTML`
%   - `rdf:XMLLiteral`
%   - The XSD datatypes as defined by xsd.pl.
%
% Since the RDF value → lexical form mapping
% cannot be formulated for language-tagged strings,
% this mapping is defined onto literal compound terms.
%
% Literal will never be the legacy compound term `literal(LexicalForm:atom)`.
%
% @compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/)

rdf_canonical_map(D, Val, CLit):-
  rdf_canonical_map(D, Val, CLex, CLTags),
  rdf_literal_components(CLit, D, CLex, CLTags).


%! rdf_canonical_map(
%!   +Datatype:iri,
%!   +Value,
%!   -CanonicalLexicalExpression:atom,
%!   -CanonicalLanguageTag:atom
%! ) is det.

rdf_canonical_map(D, Val, CLex, CLTag):-
  rdf_equal(rdf:langString, D), !,
  % First check value for groundness, otherwise the pair term is instantiated.
  ground(Val),
  Val = CLex-LTag,
  % Make sure the language-tag is valid as per BCP 47.
  atom_phrase('obs-language-tag'(CLTag0), LTag),
  atomic_list_concat(CLTag0, -, CLTag).
rdf_canonical_map(D, Val, CLex, _):-
  rdf_equal(rdf:'HTML', D), !,
  with_output_to(atom(CLex), html_write(current_output, Val, [])).
rdf_canonical_map(D, Val, CLex, _):-
  rdf_equal(rdf:'XMLLiteral', D), !,
  with_output_to(atom(CLex), xml_write(current_output, Val, [])).
%rdf_canonical_map('http://purl.org/dc/terms/RFC4646', LTag, CLex, _):- !,
%  maplist(string_atom, LTag, Subtags),
%  maplist(downcase_atom, Subtags, CSubtags),
%  atomic_list_concat(CSubtags, -, CLex).
rdf_canonical_map('http://purl.org/dc/terms/W3CDTF', DT, CLex, _):- !,
  atom_phrase(cdtf(DT), CLex).
rdf_canonical_map(D, Val, CLex, _):-
  xsd_canonical_map(D, Val, CLex).



%! rdf_compare_value(
%!   +Datatype:iri,
%!   +Order:oneof([incomparable,<,=,>]),
%!   +Value1,
%!   +Value2
%! ) is semidet.
%! rdf_compare_value(
%!   +Datatype:iri,
%!   -Order:oneof([incomparable,<,=,>]),
%!   +Value1,
%!   +Value2
%! ) is semidet.

rdf_compare_value(D, Order, V1, V2):-
  (   rdf_equal(D, rdf:'HTML')
  ;   rdf_equal(D, rdf:'XMLLiteral')
  ), !,
  compare(Order, V1, V2).
rdf_compare_value(D, Order, V1, V2):-
  xsd_compare_value(D, Order, V1, V2).



%! rdf_datatype(+Datatype:iri) is semidet.
%! rdf_datatype(-Datatype:iri) is multi.

rdf_datatype(D):-
  rdf_datatype(D, _).

%! rdf_datatype(+Datatype:iri, +PrologType) is semidet.
%! rdf_datatype(+Datatype:iri, -PrologType) is det.
%! rdf_datatype(-Datatype:iri, +PrologType) is nondet.
%! rdf_datatype(-Datatype:iri, -PrologType) is nondet.

rdf_datatype(rdf:langString, pair).
rdf_datatype(rdf:'HTML', compound).
rdf_datatype(rdf:'XMLLiteral', compound).
rdf_datatype('http://purl.org/dc/terms/W3CDTF', dateTime).
rdf_datatype(D, Type):-
  xsd_datatype(D, Type).



%! rdf_equiv_value(+Datatype:iri, +Value1, +Value2) is semidet.
% RDF typed literal value equivalence w.r.t. a datatype.

rdf_equiv_value(D, Val1, Val2):-
  rdf_compare_value(D, =, Val1, Val2).



%! rdf_guess_datatype(+Value, -Datatype:iri) is semidet.

rdf_guess_datatype(Val, D):-
  % First checkout for groundness.
  ground(Val),
  Val = [element(Root,_,_)], !,
  (   Root == html
  ->  rdf_equal(rdf:'HTML', D)
  ;   rdf_equal(rdf:'XMLLiteral', D)
  ).
rdf_guess_datatype(Val, D):-
  % First checkout for groundness.
  ground(Val),
  Val = Lex-LTag,
  maplist(atom, [Lex,LTag]), !,
  rdf_equal(rdf:langString, D).
rdf_guess_datatype(Val, D):-
  xsd_guess_datatype(Val, D).



%! rdf_interpreted_term(+Term1:rdf_term, -Term2) is det.

rdf_interpreted_term(X, Y):-
  is_list(X), !,
  maplist(rdf_interpreted_term, X, Y).
rdf_interpreted_term(X, X):-
  is_iri(X), !.
rdf_interpreted_term(X, X):-
  rdf_is_bnode(X), !.
rdf_interpreted_term(X, Y):-
  rdf_lexical_map(X, Y).



%! rdf_lexical_canonical_map(
%!   +Literal:compound,
%!   -CanonicalLiteral:compound
%! ) is det.
% Since the RDF lexical form → canonical lexical form mapping
% cannot be formulated for language-tagged strings,
% this mapping is defined between literal compound terms.

rdf_lexical_canonical_map(Lit, CLit):-
  rdf_literal_components(Lit, D, Lex, LTag),
  rdf_lexical_map(D, Lex, LTag, Val),
  rdf_canonical_map(D, Val, CLit).


%! rdf_lexical_canonical_map(
%!   +Datatype:iri,
%!   +LexicalExpression:atom,
%!   +LanguageTag:atom,
%!   +CanonicalLexicalExpression:atom,
%!   +CanonicalLanguageTag:atom
%! ) is det.

rdf_lexical_canonical_map(D, Lex, LTag, CLex, CLTag):-
  rdf_lexical_map(D, Lex, LTag, Val),
  rdf_canonical_map(D, Val, CLex, CLTag).


    
%! rdf_lexical_map(+Literal:compound, -Value) is det.
% Wrapper around rdf_lexical_map/4.

rdf_lexical_map(Lit, Val):-
  rdf_literal_components(Lit, D, Lex, LTag),
  rdf_lexical_map(D, Lex, LTag, Val).


%! rdf_lexical_map(+Datatype:iri, +LexicalExpression:atom, -Value) is det.
% Wrapper around rdf_lexical_map/4 that works for all RDF datatype IRIs
% except `rdf:langString'.

rdf_lexical_map(D, Lex, Val):-
  rdf_lexical_map(D, Lex, _, Val).


%! rdf_lexical_map(
%!   +Datatype:iri,
%!   +LexicalExpression:atom,
%!   +LanguageTag:atom,
%!   -Value
%! ) is det.
% Maps lexical forms onto the values they represent.
%
% Supports the following RDF datatypes:
%   - `rdf:HTML`
%   - `rdf:XMLLiteral`
%   - The XSD datatypes as defined by xsd.pl.
%
% Since the RDF lexical form → canonical lexical form mapping
% cannot be formulated for language-tagged strings,
% this mapping is defined from literal compound terms.
%
% @compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/)

% Language-tagged string.
rdf_lexical_map(D, Lex, LTag0, Lex-LTag):-
  rdf_equal(rdf:langString, D), !,
  downcase_atom(LTag0, LTag).
% Typed literal (as per RDF 1.0 specification).
rdf_lexical_map(D, Lex, _, Val):-
  rdf_equal(rdf:'HTML', D), !,
  call_collect_messages(atom_to_html_dom(Lex, Val)).
rdf_lexical_map(D, Lex, _, Val):-
  rdf_equal(rdf:'XMLLiteral', D), !,
  call_collect_messages(atom_to_xml_dom(Lex, Val)).
rdf_lexical_map('http://purl.org/dc/terms/W3CDTF', Lex, _, DT):- !,
  atom_phrase(ldtf(DT), Lex).
%rdf_canonical_map('http://purl.org/dc/terms/RFC4646', Lex, _, LTag):- !,
%  atom_phrase('Language-Tag'(LTag), Lex).
rdf_lexical_map(D, Lex, _, Val):-
  xsd_lexical_map(D, Lex, Val).



%! rdf_subtype_of(+Subtype:iri, +Supertype:iri) is semidet.
%! rdf_subtype_of(+Subtype:iri, -Supertype:iri) is nondet.
%! rdf_subtype_of(-Subtype:iri, +Supertype:iri) is nondet.
%! rdf_subtype_of(-Subtype:iri, -Supertype:iri) is multi.

rdf_subtype_of(X, Y):-
  rdfs_subclass_of(X, Y).
rdf_subtype_of(X, Y):-
  xsd_subtype_of(X, Y).
