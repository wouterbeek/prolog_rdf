:- module(
  rdf_datatype,
  [
    rdf_canonical_map/3, % +Datatype:iri
                         % +Value
                         % ?Literal:compound
    rdf_compare_value/4, % +Datatype:iri
                         % -Order:oneof([incomparable,<,=,>])
                         % +Value1
                         % +Value2
    rdf_datatype/1, % ?Datatype:iri
    rdf_datatype/2, % ?Datatype:iri
                    % ?PrologType
    rdf_datatype_term/1, % ?Datatype:iri
    rdf_datatype_term/2, % ?Datatype:iri
                         % ?Graph:atom
    rdf_equiv_value/3, % +Datatype:iri
                       % +Value1
                       % +Value2
    rdf_guess_datatype/2, % +Value
                          % -Datatype:iri
    rdf_interpreted_term/2, % +Term1:rdf_term
                            % -Term2
    rdf_lexical_canonical_map/2, % +Literal:compound
                                 % ?CanonicalLexicalFrom:atom
    rdf_lexical_map/2, % +Literal:compound
                       % ?Value
    rdf_subtype_of/2 % ?SubType:iri
                     % ?SuperType:iri
  ]
).

/** <module> RDF datatype

@author Wouter Beek
@compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/)
@license MIT License
@version 2015/07-2015/09, 2015/11
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(html/html_dom)).
:- use_module(library(ltag/rfc5646)).
:- use_module(library(memfile)).
:- use_module(library(rdf/rdf_literal)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(typecheck)).
:- use_module(library(xml/xml_dom)).
:- use_module(library(xsd/xsd)).
:- use_module(library(xsd/xsd_update)).

:- rdf_meta(rdf_canonical_map(r,+,?)).
:- rdf_meta(rdf_compare_value(r,?,+,+)).
:- rdf_meta(rdf_datatype(r)).
:- rdf_meta(rdf_datatype(r,?)).
:- rdf_meta(rdf_datatype_term(r)).
:- rdf_meta(rdf_datatype_term(r,?)).
:- rdf_meta(rdf_equiv_value(r,+,+)).
:- rdf_meta(rdf_interpreted_term(o,-)).
:- rdf_meta(rdf_lexical_canonical_map(o,?)).
:- rdf_meta(rdf_lexical_map(o,?)).
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

rdf_canonical_map(D, Val, literal(lang(LTag,Lex))):-
  rdf_equal(rdf:langString, D), !,
  % First check value for groundness, otherwise the pair term is instantiated.
  ground(Val),
  Val = Lex-LTag0,
  % Make sure the language-tag is valid as per BCP 47.
  atom_phrase('obs-language-tag'(LTagStrings), LTag0),
  atomic_list_concat(LTagStrings, -, LTag).
rdf_canonical_map(D, Val, literal(type(D,Lex))):-
  (   rdf_equal(rdf:'HTML', D)
  ->  with_output_to(atom(Lex), html_write(current_output, Val, []))
  ;   rdf_equal(rdf:'XMLLiteral', D)
  ->  with_output_to(atom(Lex), xml_write(current_output, Val, []))
  ;   xsd_canonical_map(D, Val, Lex)
  ).



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
rdf_datatype(D, Type):-
  xsd_datatype(D, Type).



%! rdf_datatype_term(+Datatype:iri) is semidet.
%! rdf_datatype_term(-Datatype:iri) is nondet.

rdf_datatype_term(D):-
  rdf_datatype_term(D, _).

%! rdf_datatype_term(+Datatype:iri, +Graph:atom) is semidet.
%! rdf_datatype_term(+Datatype:iri, -Graph:atom) is nondet.
%! rdf_datatype_term(-Datatype:iri, +Graph:atom) is nondet.
%! rdf_datatype_term(-Datatype:iri, -Graph:atom) is nondet.

rdf_datatype_term(D, G):-
  user:rdf(_, _, literal(type(D,_)), G).
rdf_datatype_term(D, G):-
  rdfs_instance(D, rdfs:'Datatype'),
  rdf_term(D, G).



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

rdf_lexical_canonical_map(Lit1, Lit2):-
  rdf_lexical_map(Lit1, D, Val),
  rdf_canonical_map(D, Val, Lit2).



%! rdf_lexical_map(+Literal:compound, +Datatype:iri, +Value) is semidet.
%! rdf_lexical_map(+Literal:compound, -Datatype:iri, -Value) is det.
% Wrapper around rdf_lexical_map/3.

rdf_lexical_map(Lit, Val):-
  rdf_lexical_map(Lit, _, Val).


%! rdf_lexical_map(+Literal:compound, +Datatype:iri, +Value) is semidet.
%! rdf_lexical_map(+Literal:compound, -Datatype:iri, -Value) is det.
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

% Typed literal (as per RDF 1.0 specification).
rdf_lexical_map(literal(type(D,Lex)), D, Val):- !,
  (   rdf_global_id(rdf:'HTML', D)
  ->  atom_to_html_dom(Lex, Val)
  ;   rdf_global_id(rdf:'XMLLiteral', D)
  ->  atom_to_xml_dom(Lex, Val)
  ;   xsd_lexical_map(D, Lex, Val)
  ).
% Language-tagged string.
rdf_lexical_map(literal(lang(LTag0,Lex)), D, Lex-LTag):- !,
  rdf_global_id(rdf:langString, D),
  downcase_atom(LTag0, LTag).
% Simple literal (as per RDF 1.0 specification)
% now assumed to be of type `xsd:string` (as per RDF 1.1 specification).
rdf_lexical_map(literal(Lex), D, Val):-
  rdf_global_id(xsd:string, D),
  rdf_lexical_map(literal(type(D,Lex)), Val).



%! rdf_subtype_of(+Subtype:iri, +Supertype:iri) is semidet.
%! rdf_subtype_of(+Subtype:iri, -Supertype:iri) is nondet.
%! rdf_subtype_of(-Subtype:iri, +Supertype:iri) is nondet.
%! rdf_subtype_of(-Subtype:iri, -Supertype:iri) is multi.

rdf_subtype_of(X, Y):-
  rdfs_subclass_of(X, Y).
rdf_subtype_of(X, Y):-
  xsd_subtype_of(X, Y).
