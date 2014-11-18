:- module(
  rdf_build_legacy,
  [
    rdf_assert_plain_literal/5, % +Term:rdf_term
                                % +Predicate:iri
                                % +Value
                                % ?LangTag:list(atom)
                                % ?Graph:atom
    rdf_assert_plain_literal/6, % +Term:rdf_term
                                % +Predicate:iri
                                % +Value
                                % ?LangTag:list(atom)
                                % ?Graph:atom
                                % -Triple:compound
    rdf_assert_simple_literal/4, % +Term:rdf_term
                                 % +Predicate:iri
                                 % +Value
                                 % ?Graph:atom
    rdf_assert_simple_literal/5, % +Term:rdf_term
                                 % +Predicate:iri
                                 % +Value
                                 % ?Graph:atom
                                 % -Triple:compound
    rdf_assert_typed_literal/5, % +Term:rdf_term
                                % +Predicate:iri
                                % +Value
                                % +Datatype:iri
                                % ?Graph:atom
    rdf_assert_typed_literal/6 % +Term:rdf_term
                               % +Predicate:iri
                               % +Value
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
%!   +Value,
%!   ?LangTag:list(atom),
%!   ?Graph:atom
%! ) is det.
%! rdf_assert_plain_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +Value,
%!   ?LangTag:list(atom),
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a plain literal.

rdf_assert_plain_literal(Term, P, Value, LangTag, Graph):-
  rdf_assert_plain_literal(Term, P, Value, LangTag, Graph, _).

rdf_assert_plain_literal(Term, P, Value, LangTag, Graph, Triple):-
  (   var(LangTag)
  ->  rdf_equal(Datatype, xsd:string)
  ;   rdf_equal(Datatype, rdf:langTag)
  ),
  rdf_assert_literal(Term, P, Value, Datatype, LangTag, Graph, Triple).



%! rdf_assert_simple_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +Value,
%!   ?Graph:atom
%! ) is det.
%! rdf_assert_simple_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +Value,
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a simple literal.

rdf_assert_simple_literal(Term, P, Value, Graph):-
  rdf_assert_simple_literal(Term, P, Value, Graph, _).

rdf_assert_simple_literal(Term, P, Value, Graph, Triple):-
  rdf_assert_literal(Term, P, Value, xsd:string, _, Graph, Triple).



%! rdf_assert_typed_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +Value,
%!   +Datatype:iri,
%!   ?Graph:atom
%! ) is det.
%! rdf_assert_typed_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +Value,
%!   +Datatype:iri,
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a typed literal.

rdf_assert_typed_literal(Term, P, Value, Datatype, Graph):-
  rdf_assert_typed_literal(Term, P, Value, Datatype, Graph, _).

rdf_assert_typed_literal(Term, P, Value, Datatype, Graph, Triple):-
  rdf_assert_literal(Term, P, Value, Datatype, _, Graph, Triple).

