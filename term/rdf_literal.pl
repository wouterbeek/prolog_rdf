:- module(
  rdf_literal,
  [
    rdf_is_langstring/1, % @Term
    rdf_is_plain_literal/1, % @Term
    rdf_is_simple_literal/1, % @Term
    rdf_is_typed_literal/1, % @Term
    rdf_langstring_data/3, % ?Field:atom
                           % +Literal:compound
                           % ?Data
    rdf_langstring_term/1, % ?Literal:compound
    rdf_langstring_term/2, % ?Literal:compound
                           % ?Graph:atom
    rdf_literal_data/3, % ?Field:atom
                        % +Literal:compound
                        % ?Data
    rdf_literal_equiv/2, % +Literal1:compound
                         % +Literal2:compound
    rdf_literal_term/1, % ?Literal:compound
    rdf_literal_term/2, % ?Literal:compound
                        % ?Graph:atom
    rdf_plain_literal_data/3, % ?Field:atom
                              % +Literal:compound
                              % ?Data
    rdf_plain_literal_term/1, % ?Literal:compound
    rdf_plain_literal_term/2, % ?Literal:compound
                              % ?Graph:atom
    rdf_simple_literal_data/3, % ?Field:atom
                               % +Literal:compound
                               % ?Data
    rdf_simple_literal_term/1, % ?Literal:compound
    rdf_simple_literal_term/2, % ?Literal:compound
                               % ?Graph:atom
    rdf_typed_literal_data/3, % ?Field:atom
                              % +Literal:compound
                              % ?Data
    rdf_typed_literal_term/1, % ?Literal:compound
    rdf_typed_literal_term/2 % ?Literal:compound
                             % ?Graph:atom
  ]
).

/** <module> RDF literal

# RDF 1.1 Literals

The **language-tagged strings** are the cartesian product of the Unicode
 strings in Normal Form C with the set of BCP 47 language tags.

# RDF 1.0 Literals

Support for RDF 1.0 simple, plain, and typed literals.
Simple, plain, and typed literals are obsolete in RDF 1.1.

The **simple literals** used to be defined as the Unicode strings
 in Normal Form C.

The **plain literals** used to be defined as the union of the Unicode strings
 in Normal Form C and the cartesian product of the Unicode strings
 in Normal Form C with the set of BCP 47 language tags.

The **typed literals** used to be defined as the cartesian product of
 the Unicode strings in Normal Form C with the set of datatype URIs.

---

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(typecheck)).

:- use_module(plRdf(term/rdf_datatype)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_is_literal(o)).
:- rdf_meta(rdf_is_typed_literal(o)).
:- rdf_meta(rdf_langstring_data(?,+,r)).
:- rdf_meta(rdf_literal_data(+,o,r)).
:- rdf_meta(rdf_literal_equiv(r,o,o)).
:- rdf_meta(rdf_literal_term(o)).
:- rdf_meta(rdf_literal_term(o,?)).
:- rdf_meta(rdf_lexical_map(?,r,?,?)).
:- rdf_meta(rdf_plain_literal_data(?,o,r)).
:- rdf_meta(rdf_simple_literal_date(?,o,r)).
:- rdf_meta(rdf_typed_literal_data(?,o,r)).
:- rdf_meta(rdf_typed_literal_term(o)).
:- rdf_meta(rdf_typed_literal_term(o,?)).



%! rdf_is_langstring(@Term) is semidet.
% Succeeds on language-tagged strings.

rdf_is_langstring(literal(lang(LangTag,LexicalForm))):-
  atom(LangTag),
  atom(LexicalForm).



%! rdf_is_plain_literal(@Term) is semidet.
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



%! rdf_langstring_data(+Field:atom, +Literal:compound, +Data) is semidet.
%! rdf_langstring_data(+Field:atom, +Literal:compound, -Data) is det.
%! rdf_langstring_data(-Field:atom, +Literal:compound, -Data) is multi.
% Decomposes language-tagged strings.
%
% Field is one of:
%   - `datatype`
%   - `langtag`
%   - `lexical_form`
%   - `value`

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



%! rdf_langstring_term(+LangTaggedString:compound, +Graph:atom) is semidet.
%! rdf_langstring_term(+LangTaggedString:compound, -Graph:atom) is semidet.
%! rdf_langstring_term(-LangTaggedString:compound, +Graph:atom) is semidet.
%! rdf_langstring_term(-LangTaggedString:compound, -Graph:atom) is semidet.

rdf_langstring_term(Literal, Graph):-
  Literal = literal(lang(_,_)),
  rdf_current_literal(Literal),
  rdf_object(Literal, Graph).



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

rdf_literal_data(datatype, literal(type(Datatype,_)), Datatype).
rdf_literal_data(datatype, literal(lang(_,_)), rdf:langString).
rdf_literal_data(datatype, literal(LexicalForm), xsd:string):-
  atom(LexicalForm).
rdf_literal_data(langtag, literal(lang(LangTag0,_)), LangTag):-
  atomic_list_concat(LangTag, '-', LangTag0).
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



%! rdf_literal_term(+Literal:compound, +Graph:atom) is semidet.
%! rdf_literal_term(+Literal:compound, -Graph:atom) is nondet.
%! rdf_literal_term(-Literal:compound, +Graph:atom) is nondet.
%! rdf_literal_term(-Literal:compound, -Graph:atom) is nondet.

rdf_literal_term(Literal, Graph):-
  rdf_current_literal(Literal),
  rdf_object(Literal, Graph).



%! rdf_plain_literal_data(+Field:atom, +Literal:compound, +Data) is semidet.
%! rdf_plain_literal_data(+Field:atom, +Literal:compound, -Data) is det.
%! rdf_plain_literal_data(-Field:atom, +Literal:compound, -Data) is multi.

rdf_plain_literal_data(datatype, literal(lang(_,_)), rdf:langString).
rdf_plain_literal_data(datatype, literal(LexicalForm), xsd:string):-
  atom(LexicalForm).
rdf_plain_literal_data(langtag, literal(lang(Data,_)), Data).
rdf_plain_literal_data(lexical_from, literal(lang(_,Data)), Data).
rdf_plain_literal_data(lexical_from, literal(Data), Data):-
  atom(Data).
rdf_plain_literal_data(value, literal(lang(LangTag,LexicalForm)), Data):-
  Data = LangTag-LexicalForm.
rdf_plain_literal_data(value, literal(Data), Data):-
  atom(Data).



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



%! rdf_simple_literal_data(+Field:atom, +Literal:compound, +Data) is semidet.
%! rdf_simple_literal_data(+Field:atom, +Literal:compound, -Data) is det.
%! rdf_simple_literal_data(-Field:atom, +Literal:compound, -Data) is multi.

rdf_simple_literal_data(datatype, literal(LexicalForm), xsd:string):-
  atom(LexicalForm).
rdf_simple_literal_data(lexical_form, literal(Data), Data):-
  atom(Data).
rdf_simple_literal_data(value, literal(Data), Data):-
  atom(Data).



%! rdf_simple_literal_term(+Literal:compound) is semidet.
%! rdf_simple_literal_term(-Literal:compound) is nondet.
% Plain literal compound terms, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_simple_literal_term(Literal):-
  % Enumerate all literals.
  rdf_current_literal(Literal),
  % Make sure the literal is simple.
  rdf_is_simple_literal(Literal).



%! rdf_simple_literal_term(+Literal:compound, +Graph:atom) is semidet.
%! rdf_simple_literal_term(+Literal:compound, -Graph:atom) is nondet.
%! rdf_simple_literal_term(-Literal:compound, +Graph:atom) is nondet.
%! rdf_simple_literal_term(-Literal:compound, -Graph:atom) is nondet.
% Pairs of RDF graphs to simple literals.
% Enumeration is assured to not deliver any duplicates.

rdf_simple_literal_term(Literal, Graph):-
  rdf_simple_literal_term(Literal),
  rdf_object(Literal, Graph).



%! rdf_typed_literal_data(+Field:atom, +Literal:compound, +Data) is semidet.
%! rdf_typed_literal_data(+Field:atom, +Literal:compound, -Data) is det.
%! rdf_typed_literal_data(-Field:atom, +Literal:compound, -Data) is multi.

rdf_typed_literal_data(datatype, literal(type(Data,_)), Data).
rdf_typed_literal_data(lexical_form, literal(type(_,Data)), Data).
rdf_typed_literal_data(value, literal(type(Datatype,LexicalForm)), Data):-
  rdf_lexical_map(Datatype, LexicalForm, Data).



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
