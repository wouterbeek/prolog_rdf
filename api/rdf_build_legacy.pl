:- module(
  rdf_build_legacy,
  [
    rdf_assert_plain_literal/6, % +Term:rdf_term
                                % +Predicate:iri
                                % +LexicalForm:atom
                                % ?LangTag:list(atom)
                                % ?Graph:atom
                                % -Triple:compound
    rdf_assert_simple_literal/5, % +Term:rdf_term
                                 % +Predicate:iri
                                 % +LexicalForm:atom
                                 % ?Graph:atom
                                 % -Triple:compound
    rdf_assert_typed_literal/5 % +Term:rdf_term
                               % +Predicate:iri
                               % +LexicalForm:atom
                               % +Datatype:iri
                               % ?Graph:atom
                               % -Triple:compound
  ]
).

/** <module> RDF API: Build legacy RDF constructs

@author Wouter Beek
@compat RDF 1.0
@version 2014/11
*/

:- use_module(plRdf(api/rdf_build)).

:- rdf_meta(rdf_assert_plain_literal(o,r,+,?,?,-)).
:- rdf_meta(rdf_assert_simple_literal(o,r,+,?,-)).
:- rdf_meta(rdf_assert_typed_literal(o,r,+,r,?,-)).



%! rdf_assert_plain_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   ?LangTag:list(atom)
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a plain literal.

rdf_assert_plain_literal(Term, P, LexicalForm, LangTag, Graph, Triple):-
  (   var(LangTag)
  ->  rdf_equal(Datatype, xsd:string)
  ;   rdf_equal(Datatype, rdf:langTag)
  ),
  rdf_assert_literal(Term, P, LexicalForm, Datatype, LangTag, Graph, Triple).



%! rdf_assert_simple_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a simple literal.

rdf_assert_simple_literal(Term, P, LexicalForm, Graph, Triple):-
  rdf_assert_literal(Term, P, LexicalForm, xsd:string, _, Graph, Triple).



%! rdf_assert_typed_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   +Datatype:iri,
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a typed literal.

rdf_assert_typed_literal(Term, P, LexicalForm, Datatype, Graph, Triple):-
  rdf_assert_literal(Term, P, LexicalForm, Datatype, _, Graph, Triple).
