:- module(
  rdf_plain_literal,
  [
    rdf_plain_literal/1, % ?PlainLiteral:compound
    rdf_plain_literal/2, % ?PlainLiteral:compound
                         % ?RdfGraph:atom
    rdf_plain_literal/3 % ?PlainLiteral:compound
                        % ?LexicalForm:atom
                        % ?LangTag:atom
  ]
).

/** <module> RDF plain literal

Support for RDF 1.0 plain literals.
Plain literals are obsolete in RDF 1.1.

A plain literal used to be defined as the union of the Unicode strings
in Normal Form C and the cartesian product of the Unicode strings
in Normal Form C with the set of BCP 47 language tags.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_simple_literal)).
:- use_module(rdf_term(rdf_term)).

:- rdf_meta(rdf_plain_literal(o)).
:- rdf_meta(rdf_plain_literal(o,?)).
:- rdf_meta(rdf_plain_literal(o,?,?)).



%! rdf_is_plain_literal(+RdfTerm:or([bnode,iri,literal])) is semidet.

% Simple plain literal.
rdf_is_plain_literal(SimpleLiteral):-
  rdf_simple_literal:rdf_is_simple_literal(SimpleLiteral).
% Non-simple plain literal.
rdf_is_plain_literal(literal(lang(LangTag,LexicalForm))):-
  atom(LangTag),
  atom(LexicalForm).


%! rdf_plain_literal(+PlainLiteral:compound) is semidet.
%! rdf_plain_literal(-PlainLiteral:compound) is nondet.
% Plain literal compound terms, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_plain_literal(PlainLiteral):-
  % Enumerate all literals.
  rdf_current_literal(PlainLiteral),
  % Make sure the literal is plain.
  rdf_is_plain_literal(PlainLiteral).


%! rdf_plain_literal(+PlainLiteral:compound, +RdfGraph:atom) is semidet.
%! rdf_plain_literal(-PlainLiteral:compound, +RdfGraph:atom) is nondet.
%! rdf_plain_literal(+PlainLiteral:compound, -RdfGraph:atom) is nondet.
%! rdf_plain_literal(-PlainLiteral:compound, -RdfGraph:atom) is nondet.
% Pairs of RDF graphs to plain literals.
% Enumeration is assured to not deliver any duplicates.

rdf_plain_literal(PlainLiteral, G):-
  rdf_plain_literal(PlainLiteral),
  rdf_object(PlainLiteral, G).


%! rdf_plain_literal(+PlainLiteral:compound, +LexicalForm:atom, ?LangTag:atom) is semidet.
%! rdf_plain_literal(+PlainLiteral:compound, -LexicalForm:atom, -LangTag:atom) is det.
%! rdf_plain_literal(-PlainLiteral:compound, +LexicalForm:atom, ?LangTag:atom) is det.
%! rdf_plain_literal(-PlainLiteral:compound, -LexicalForm:atom, -LangTag:atom) is nondet.
% Relates a plain literal to its constituent parts:
% a lexical form and an optional language tag.
%
% ### Mode enumeration
%
% Mode (-,-,-) enumerates the asserted language-tagged strings.
% The other modes compose/decompose language-tagged strings without
% them having to exist in the store.

rdf_plain_literal(PlainLiteral, LexicalForm, LangTag):-
  var(PlainLiteral),
  var(LexicalForm),
  var(LangTag), !,
  % Enumerate all plain literals.
  rdf_plain_literal(PlainLiteral),
  rdf_plain_literal_compound(PlainLiteral, LexicalForm, LangTag).
rdf_plain_literal(PlainLiteral, LexicalForm, LangTag):-
  var(PlainLiteral), !,
  (
    var(LangTag)
  ->
    PlainLiteral = literal(LexicalForm)
  ;
    PlainLiteral = literal(lang(LangTag,LexicalForm))
  ).
rdf_plain_literal(PlainLiteral, LexicalForm, LangTag):-
  rdf_plain_literal_compound(PlainLiteral, LexicalForm, LangTag).

rdf_plain_literal_compound(PlainLiteral, LexicalForm, _):-
  rdf_simple_literal(PlainLiteral), !,
  rdf_simple_literal_lexical_form(PlainLiteral, LexicalForm).
rdf_plain_literal_compound(literal(lang(LangTag,LexicalForm)), LexicalForm, LangTag).

