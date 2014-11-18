:- module(
  rdf_read_legacy,
  [
    rdf_is_plain_literal_term/1, % @Term
    rdf_is_simple_literal_term/1, % @Term
    rdf_is_typed_literal_term/1, % @Term
    rdf_simple_literal/4, % ?Term:rdf_term
                          % ?Predicate:iri
                          % ?Value:atom
                          % ?Graph:atom
    rdf_simple_literal_data/3, % ?Field:oneof([lexical_form,value])
                               % +Literal:compound
                               % ?Data
    rdf_simple_literal_term/1, % ?Literal:compound
    rdf_simple_literal_term/2, % ?Literal:compound
                               % ?Graph:atom
    rdf_typed_literal/5, % ?Term:rdf_term
                         % ?Predicate:iri
                         % ?Value
                         % ?Datatype:iri
                         % ?Graph:atom
    rdf_typed_literal_data/3, % ?Field:oneof([datatype,lexical_form,value])
                              % +Literal:compound
                              % ?Data
    rdf_typed_literal_term/1, % ?Literal:compound
    rdf_typed_literal_term/2 % ?Literal:compound
                             % ?Graph:atom
  ]
).

/** <module> RDF typed literal

Support for RDF 1.0 simple, plain, and typed literals.
Simple, plain, and typed literals are obsolete in RDF 1.1.

A **plain literal** used to be defined as the union of the Unicode strings
in Normal Form C and the cartesian product of the Unicode strings
in Normal Form C with the set of BCP 47 language tags.

A **typed literal** used to be defined as the cartesian product of
the Unicode strings in Normal Form C with the set of datatype URIs.

@author Wouter Beek
@compat RDF 1.0
@version 2014/11
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(typecheck)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(term/rdf_literal)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_is_typed_literal(o)).
:- rdf_meta(rdf_simple_literal(o,r,?,r,?)).
:- rdf_meta(rdf_typed_literal(o,r,?,r,?)).
:- rdf_meta(rdf_typed_literal_data(?,o,-)).
:- rdf_meta(rdf_typed_literal_term(o)).
:- rdf_meta(rdf_typed_literal_term(o,?)).



%! rdf_is_simple_literal(@Term) is semidet.
% Succeeds if the given Term denotes a plain literal.

rdf_is_plain_literal(Term):-
  rdf_is_simple_literal(Term).
rdf_is_plain_literal(Term):-
  rdf_is_langstring(Term).


%! rdf_is_simple_literal(@Term) is semidet.
% Succeeds if the given Term denotes a plain literal.

rdf_is_simple_literal(literal(LexicalForm)):-
  atom(LexicalForm).



%! rdf_is_typed_literal(@Term) is semidet.
% Succeeds if the given Term denotes a typed literal.

rdf_is_typed_literal(literal(type(Datatype,LexicalForm))):-
  is_uri(Datatype),
  atom(LexicalForm).



%! rdf_plain_literal(
%!   ?Term:rdf_term,
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?LangTag:list(atom),
%!   ?Graph:atom
%! ) is nondet.

rdf_plain_literal(Term, Predicate, LexicalForm, LangTag, Graph):-
  rdf_literal(Term, Predicate, LexicalForm, _, LangTag, Graph).



%! rdf_plain_literal_term(+Literal:compound) is semidet.
%! rdf_plain_literal_term(-Literal:compound) is nondet.
% Plain literal compound terms, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_plain_literal_term(Literal):-
  % Enumerate all literals.
  rdf_current_literal(Literal),
  % Make sure the literal is plain.
  rdf_is_plain_literal(Literal).


%! rdf_plain_literal_term(+Literal:compound, +Graph:atom) is semidet.
%! rdf_plain_literal_term(-Literal:compound, +Graph:atom) is nondet.
%! rdf_plain_literal_term(+Literal:compound, -Graph:atom) is nondet.
%! rdf_plain_literal_term(-Literal:compound, -Graph:atom) is nondet.
% Pairs of RDF graphs to plain literals.
% Enumeration is assured to not deliver any duplicates.

rdf_plain_literal_term(Literal, Graph):-
  rdf_plain_literal_term(Literal),
  rdf_object(Literal, Graph).



%! rdf_simple_literal(
%!   ?Term:rdf_term,
%!   ?Predicate:iri,
%!   ?Value:atom,
%!   ?Graph:atom
%! ) is nondet.

rdf_simple_literal(Term, Predicate, Value, Graph):-
  rdf_literal(Term, Predicate, Value, xsd:string, _, Graph).



%! rdf_simple_literal_data(
%!   +Field:oneof([lexical_form,value]),
%!   +Literal:compound,
%!   +Data
%! ) is semidet.
%! rdf_simple_literal_data(
%!   +Field:oneof([lexical_form,value]),
%!   +Literal:compound,
%!   -Data
%! ) is det.
%! rdf_simple_literal_data(
%!   -Field:oneof([lexical_form,value]),
%!   +Literal:compound,
%!   -Data
%! ) is multi.

rdf_simple_literal_data(lexical_form, literal(LexicalForm), LexicalForm).
rdf_simple_literal_data(value, literal(Value), Value).



%! rdf_simple_literal_term(+Literal:compound) is semidet.
%! rdf_simple_literal_term(-Literal:compound) is nondet.
% Plain literal compound terms, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_typed_literal_term(Literal):-
  % Enumerate all literals.
  rdf_current_literal(Literal),
  % Make sure the literal is typed.
  rdf_is_typed_literal(Literal).



%! rdf_simple_literal_term(+Literal:compound, +Graph:atom) is semidet.
%! rdf_simple_literal_term(+Literal:compound, -Graph:atom) is nondet.
%! rdf_simple_literal_term(-Literal:compound, +Graph:atom) is nondet.
%! rdf_simple_literal_term(-Literal:compound, -Graph:atom) is nondet.
% Pairs of RDF graphs to simple literals.
% Enumeration is assured to not deliver any duplicates.

rdf_simple_literal_term(Literal, Graph):-
  rdf_simple_literal_term(Literal),
  rdf_object(Literal, Graph).



%! rdf_typed_literal(
%!   ?Term:rdf_term,
%!   ?Predicate:iri,
%!   ?Value,
%!   ?Datatype:iri,
%!   ?Graph:atom
%! ) is nondet.

rdf_typed_literal(Term, Predicate, Value, Datatype, Graph):-
  rdf_literal(Term, Predicate, Value, Datatype, _, Graph).



%! rdf_typed_literal_data(
%!   +Field:oneof([datatype,lexical_form,value]),
%!   +Literal:compound,
%!   +Data
%! ) is semidet.
%! rdf_typed_literal_data(
%!   +Field:oneof([datatype,lexical_form,value]),
%!   +Literal:compound,
%!   -Data
%! ) is det.
%! rdf_typed_literal_data(
%!   -Field:oneof([datatype,lexical_form,value]),
%!   +Literal:compound,
%!   -Data
%! ) is multi.

rdf_typed_literal_data(datatype, literal(type(Datatype,_)), Datatype).
rdf_typed_literal_data(
  lexical_form,
  literal(type(_,LexicalForm)),
  LexicalForm
).
rdf_typed_literal_data(value, literal(type(Datatype,LexicalForm)), Value):-
  rdf_lexical_map(Datatype, LexicalForm, Value).



%! rdf_typed_literal_term(+Literal:compound) is semidet.
%! rdf_typed_literal_term(-Literal:compound) is nondet.
% Typed literal compound terms, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_typed_literal_term(Literal):-
  % Enumerate all literals.
  rdf_current_literal(Literal),
  % Make sure the literal is typed.
  rdf_is_typed_literal(Literal).



%! rdf_typed_literal_term(+Literal:compound, +Graph:atom) is semidet.
%! rdf_typed_literal_term(+Literal:compound, -Graph:atom) is nondet.
%! rdf_typed_literal_term(-Literal:compound, +Graph:atom) is nondet.
%! rdf_typed_literal_term(-Literal:compound, -Graph:atom) is nondet.
% Pairs of RDF graphs to typed literals.
% Enumeration is assured to not deliver any duplicates.

rdf_typed_literal_term(Literal, Graph):-
  rdf_typed_literal_term(Literal),
  rdf_object(Literal, Graph).
