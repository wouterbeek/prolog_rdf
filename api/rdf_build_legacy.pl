:- module(
  rdf_build_legacy,
  [
    rdf_assert_plain_literal/4, % +Term:rdf_term
                                % +Predicate:iri
                                % +LexicalForm:atom
                                % ?LangTag:list(atom)
                                % ?Graph:atom
    rdf_assert_simple_literal/4, % +Term:rdf_term
                                 % +Predicate:iri
                                 % +LexicalForm:atom
                                 % ?Graph:atom
    rdf_assert_typed_literal/5 % +Term:rdf_term
                               % +Predicate:iri
                               % +LexicalForm:atom
                               % +Datatype:iri
                               % ?Graph:atom
  ]
).

/** <module> RDF API: Build legacy RDF constructs

@author Wouter Beek
@compat RDF 1.0
@version 2014/11
*/

:- use_module(plRdf(api/rdf_build)).

:- rdf_meta(rdf_assert_plain_literal(o,r,+,?,?)).
:- rdf_meta(rdf_assert_simple_literal(o,r,+,?)).
:- rdf_meta(rdf_assert_typed_literal(o,r,+,r,?)).



%! rdf_assert_plain_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   ?LangTag:list(atom)
%!   ?Graph:atom
%! ) is det.
% Asserts a plain literal.

rdf_assert_plain_literal(Term, Predicate, LexicalForm, LangTag, Graph):-
  (   var(LangTag)
  ->  rdf_equal(Datatype, xsd:string)
  ;   rdf_equal(Datatype, rdf:langTag)
  ),
  rdf_assert_literal(Term, Predicate, LexicalForm, Datatype, LangTag, Graph).



%! rdf_assert_simple_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   ?Graph:atom
%! ) is det.
% Asserts a simple literal.

rdf_assert_simple_literal(Term, Predicate, LexicalForm, Graph):-
  rdf_assert_literal(Term, Predicate, LexicalForm, xsd:string, _, Graph).



%! rdf_assert_typed_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   +Datatype:iri,
%!   ?Graph:atom
%! ) is det.
% Asserts a typed literal.

rdf_assert_typed_literal(Term, Predicate, LexicalForm, Datatype, Graph):-
  rdf_assert_literal(Term, Predicate, LexicalForm, Datatype, _, Graph).
