:- module(
  rdf_datatype,
  [
    rdf_canonical_map/3, % +Datatype:iri
                         % +Value
                         % ?LexicalForm:atom
    rdf_compare/4, % +Datatype:iri
                   % -Order:oneof([incomparable,<,=,>])
                   % +Value1
                   % +Value2
    rdf_datatype/1, % ?Datatype:iri
    rdf_datatype/2, % ?Datatype:iri
                    % ?PrologType
    rdf_datatype_term/1, % ?Datatype:iri
    rdf_datatype_term/2, % ?Datatype:iri
                         % ?Graph:atom
    rdf_equiv/3, % +Datatype:iri
                 % +Value1
                 % +Value2
    rdf_lexical_map/2, % +Literal:compound
                       % ?Value
    rdf_lexical_map/3 % +Datatype:iri
                      % +LexicalForm:atom
                      % ?Value
  ]
).

/** <module> RDF datatype

@author Wouter Beek
@compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/)
@version 2014/11-2014/12
*/

:- use_module(library(memfile)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).

:- use_module(plXsd(xsd)).

:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_canonical_map(r,+,?)).
:- rdf_meta(rdf_compare(r,?,+,+)).
:- rdf_meta(rdf_datatype(r)).
:- rdf_meta(rdf_datatype(r,?)).
:- rdf_meta(rdf_datatype_term(r)).
:- rdf_meta(rdf_datatype_term(r,?)).
:- rdf_meta(rdf_equiv(r,+,+)).
:- rdf_meta(rdf_lexical_map(r,+,?)).





%! rdf_canonical_map(+Datatype:iri, +Value, +LexicalForm:atom) is semidet.
%! rdf_canonical_map(+Datatype:iri, +Value, -LexicalForm:atom) is det.
% Maps RDF datatyped values onto a unique / canonical lexical form.
%
% Supports the following RDF datatypes:
%   - `rdf:HTML`
%   - `rdf:XMLLiteral`
%   - The XSD datatypes as defined by xsd.pl.
%
% @compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/)

rdf_canonical_map(rdf:'HTML', Value, LexicalForm):- !,
  with_output_to(atom(LexicalForm), html_write(current_output, Value, [])).
rdf_canonical_map(rdf:'XMLLiteral', Value, LexicalForm):- !,
  with_output_to(atom(LexicalForm), xml_write(current_output, Value, [])).
rdf_canonical_map(Datatype, Value, LexicalForm):-
  xsd_canonical_map(Datatype, Value, LexicalForm).



%! rdf_compare(
%!   +Datatype:iri,
%!   +Order:oneof([incomparable,<,=,>]),
%!   +Value1,
%!   +Value2
%! ) is semidet.
%! rdf_compare(
%!   +Datatype:iri,
%!   -Order:oneof([incomparable,<,=,>]),
%!   +Value1,
%!   +Value2
%! ) is semidet.

rdf_compare(Datatype, Order, Value1, Value2):-
  (   rdf_equal(Datatype, rdf:'HTML')
  ;   rdf_equal(Datatype, rdf:'XMLLiteral')
  ), !,
  compare(Order, Value1, Value2).
rdf_compare(Datatype, Order, Value1, Value2):-
  xsd_compare(Datatype, Order, Value1, Value2).



%! rdf_datatype(+Datatype:iri) is semidet.
%! rdf_datatype(-Datatype:iri) is multi.

rdf_datatype(Datatype):-
  rdf_datatype(Datatype, _).

%! rdf_datatype(+Datatype:iri, +PrologType) is semidet.
%! rdf_datatype(+Datatype:iri, -PrologType) is det.
%! rdf_datatype(-Datatype:iri, +PrologType) is nondet.
%! rdf_datatype(-Datatype:iri, -PrologType) is nondet.

rdf_datatype(rdf:'HTML',       compound).
rdf_datatype(rdf:'XMLLiteral', compound).
rdf_datatype(rdf:langString,   pair).
rdf_datatype(Datatype,         Type):-
  xsd_datatype(Datatype, Type).



%! rdf_datatype_term(+Datatype:iri) is semidet.
%! rdf_datatype_term(-Datatype:iri) is nondet.

rdf_datatype_term(Datatype):-
  rdf_datatype_term(Datatype, _).

%! rdf_datatype_term(+Datatype:iri, +Graph:atom) is semidet.
%! rdf_datatype_term(+Datatype:iri, -Graph:atom) is nondet.
%! rdf_datatype_term(-Datatype:iri, +Graph:atom) is nondet.
%! rdf_datatype_term(-Datatype:iri, -Graph:atom) is nondet.

rdf_datatype_term(Datatype, Graph):-
  rdf(_, _, literal(type(Datatype,_)), Graph).
rdf_datatype_term(Datatype, Graph):-
  rdfs_individual_of(Datatype, rdfs:'Datatype'),
  rdf_term(Datatype, Graph).



%! rdf_equiv(+Datatype:iri, +Value1, +Value2) is semidet.
% RDF typed literal value equivalence w.r.t. a datatype.

rdf_equiv(D, V1, V2):-
  rdf_compare(D, =, V1, V2).



%! rdf_lexical_map(+Literal:compound, +Value) is semidet.
%! rdf_lexical_map(+Literal:compound, -Value) is det.

rdf_lexical_map(literal(type(Datatype,LexicalForm)), Value):- !,
  rdf_lexical_map(Datatype, LexicalForm, Value).
rdf_lexical_map(literal(lang(LangTag,LexicalForm)), LangTag-LexicalForm):- !.
rdf_lexical_map(literal(LexicalForm), Value):-
  rdf_lexical_map(xsd:string, LexicalForm, Value).



%! rdf_lexical_map(+Datatype:iri, +LexicalForm:atom, +Value) is semidet.
%! rdf_lexical_map(+Datatype:iri, +LexicalForm:atom, -Value) is det.
% Maps lexical forms onto the values they represent.
%
% Supports the following RDF datatypes:
%   - `rdf:HTML`
%   - `rdf:XMLLiteral`
%   - The XSD datatypes as defined by xsd.pl.
%
% @compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/)

rdf_lexical_map(rdf:'HTML', LexicalForm, Value):- !,
  setup_call_cleanup(
    atom_to_memory_file(LexicalForm, Handle),
    setup_call_cleanup(
      open_memory_file(Handle, read, In),
      load_html(In, Value, []),
      close(In)
    ),
    free_memory_file(Handle)
  ).
rdf_lexical_map(rdf:'XMLLiteral', LexicalForm, Value):- !,
  setup_call_cleanup(
    atom_to_memory_file(LexicalForm, Handle),
    setup_call_cleanup(
      open_memory_file(Handle, read, In),
      load_xml(In, Value, []),
      close(In)
    ),
    free_memory_file(Handle)
  ).
rdf_lexical_map(Datatype, LexicalForm, Value):-
  xsd_lexical_map(Datatype, LexicalForm, Value).
