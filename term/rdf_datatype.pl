:- module(
  rdf_datatype,
  [
    rdf_assert_datatype/5, % +Subject:oneof([bnode,iri])
                           % +Predicate:iri
                           % +Value
                           % +DatatypeIri:iri
                           % +RdfGraph:atom
    rdf_datatype/2, % ?DatatypeIri:iri
                    % ?RdfGraph:atom
    rdf_datatype/5, % ?Subject:oneof([bnode,iri])
                    % ?Predicate:iri
                    % ?Value
                    % ?DatatypeIri:iri
                    % ?RdfGraph:atom
    rdf_overwrite_datatype/5, % +Subject:oneof([bnode,iri])
                              % +Predicate:iri
                              % +LexicalForm2
                              % +DatatypeIri:iri
                              % +RdfGraph:atom
    rdf_retractall_datatype/4 % ?Subject:oneof([bnode,iri])
                              % ?Predicate:iri
                              % ?DatatypeIri:iri
                              % ?RdfGraph:atom
  ]
).

/** <module> RDF datatype

Support for RDF typed literals.

@author Wouter Beek
@version 2013/10, 2014/01-2014/03
*/

:- use_module(dcg(dcg_generic)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_name)). % Meta-DCG.
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_literal_build)).
:- use_module(rdf_term(rdf_typed_literal)).
:- use_module(xsd(xsd)).

:- rdf_meta(rdf_assert_datatype(r,r,+,r,+)).
:- rdf_meta(rdf_datatype(r,?)).
:- rdf_meta(rdf_datatype(r,r,?,r,?)).
:- rdf_meta(rdf_overwrite_datatype(r,r,+,r,+)).
:- rdf_meta(rdf_retractall_datatype(r,r,r,?)).



%! rdf_assert_datatype(
%!   +Subject:oneof([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   +DatatypeIri:iri,
%!   +RdfGraph:atom
%! ) is det.
% Asserts a datatyped value for a blank node or IRI reference.
%
% We choose to use the XML Schema 2 Datatypes (2nd Edition)
% for this. The asserted values are the atomic equivalent of the
% *|canonical lexical representations|* as defined by that standard.
%
% We only emit canonical representations for XSD values.
%
% Language-tagged strings cannot be asserted in this way.

% Simple literals.
rdf_assert_datatype(S, P, Value, DatatypeIri, G):-
  var(DatatypeIri), !,
  rdf_assert_datatype(S, P, Value, xsd:string, G).
% Others.
rdf_assert_datatype(S, P, Value, DatatypeIri, G):-
  xsd_canonical_map(DatatypeIri, Value, LexicalForm),
  rdf_assert_literal(S, P, LexicalForm, DatatypeIri, _, G).


%! rdf_datatype(?DatatypeIri:iri, ?RdfGraph:atom) is nondet.

rdf_datatype(D, G):-
  rdf_literal(_, _, _, D, _, G).


%! rdf_datatype(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?DatatypeIri:or([atom,iri]),
%!   ?RdfGraph:atom
%! ) is nondet.
% @tbd Ideally, we would like to close lexical expressions
%      under identity and equivalence in search.

rdf_datatype(S, P, Value, Datatype, G):-
  nonvar(Datatype),
  ground(Value), !,
  % @tbd Ideally, we would like to close the lexical form
  %      under identity or equivalence.
  xsd_canonical_map(Datatype, Value, LexicalForm),
  rdf_literal(S, P, LexicalForm, Datatype, _, G).
rdf_datatype(S, P, Value, Datatype, G):-
  rdf_literal(S, P, LexicalForm, Datatype, _, G),
  rdf_literal_map(LexicalForm, Datatype, _, Value).


%! rdf_overwrite_datatype(
%!   +Subject:oneof([bnode,iri]),
%!   +Predicate:iri,
%!   +LexicalForm2,
%!   +DatatypeIri:iri,
%!   +RdfGraph:atom
%! ) is det.
% The single new value is going to overwrite all old values, unless the new
% value is already asserted. In that case none of the other values gets
% retracted.

rdf_overwrite_datatype(S, P, LexicalForm2, Datatype, G):-
  % Make sure there is at most one value that would be overwritten.
  findall(
    [S,P,Datatype,LexicalForm1,G],
    rdf_datatype(S, P, LexicalForm1, Datatype, G),
    Tuples
  ),
  (
    Tuples == [], !
  ;
    Tuples = [[S,P,Datatype,LexicalForm1,G]], !
  ),

  % Remove the old value and assert the new value.
  rdf_retractall_datatype(S, P, Datatype, G),
  rdf_assert_datatype(S, P, LexicalForm2, Datatype, G),

  % DEB: Old object term.
  rdf_typed_literal(Literal1, LexicalForm1, Datatype),
  dcg_with_output_to(atom(T1), rdf_triple_name(S, P, Literal1, G)),

  % DEB: New object term.
  rdf_typed_literal(Literal2, LexicalForm2, Datatype),
  dcg_with_output_to(atom(T2), rdf_triple_name(S, P, Literal2, G)),

  % DEB: Show old and new object term in debug message.
  debug(rdf_datatype, 'Updated triple: ~w --> ~w', [T1,T2]).


%! rdf_retractall_datatype(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?DatatypeIri:iri,
%!   ?RdfGraph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a datatypes value.

rdf_retractall_datatype(S, P, DatatypeIri, G):-
  rdf_retractall_literal(S, P, _, DatatypeIri, G).

