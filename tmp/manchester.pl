:- module(
  manchester_parser,
  [
  ]
).

/** <module> OWL 2 Web Ontology Language: Manchester Syntax (Second Edition)

@author Wouter Beek
@compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
@version 2018
*/

:- use_module(library(error)).

:- use_module(library(dcg)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(semweb/sparql_parser), [
     'PNAME_LN'//1
   ]).

:- rdf_meta
   'Datatype'(r, ?, ?),
   decimalLiteral(o, ?, ?),
   integerLiteral(o, ?, ?),
   nonNegativeInteger(o, ?, ?).





%! abbreviatedIRI(-Iri:atom)// .
%
% ```
% abbreviatedIRI := a finite sequence of characters matching the PNAME_LN
%                   production of [SPARQL]
% ```

abbreviatedIRI(Iri) -->
  'PNAME_LN'(Iri).



%! annotationPropertyIri(-Iri:atom)// .
%
% ```
% annotationPropertyIRI ::= IRI
% ```

annotationPropertyIRI(Iri) -->
  'IRI'(Iri).



%! annotations(-L:list)// .
%
% ```
% annotations ::= 'Annotations:' annotationAnnotatedList
% ```

annotations(L) -->
  "Annotations:",
  annotationAnnotatedList(L).


%! annotation(-X:atom)// .
%
% ```
% annotation ::= annotationPropertyIRI annotationTarget
% ```

annotation -->
  annotationPropertyIRI(Iri),
  annotationTarget(Targer).


%! annotationTarget(-Target:rdf_term)// .
%
% ```
% annotationTarget ::= nodeID | IRI | literal
% ```

annotationTarget(BNode) --> nodeID(BNode), !.
annotationTarget(Iri) --> 'IRI'(Iri), !.
annotationTarget(Literal) --> literal(Literal).



%! ontologyDocument// .
%
% ```
% ontologyDocument ::= { prefixDeclaration } ontology
% ```

ontologyDocument -->
  'prefixDeclaration*'(L),
  ontology.

'prefixDeclaration*'([H|T]) -->
  prefixDeclaration(H), !,
  'prefixDeclaration*'(T).
'prefixDeclaration*'([]) --> "".



%! prefixDeclaration(-AliasPrefix:pair(atom))// .
%
% ```
% prefixDeclaration ::= 'Prefix:' prefixName fullIRI
% ```

prefixDeclaration(Alias-Prefix) -->
  "Prefix:",
  must_see(prefixName(Alias)),
  must_see(fullIRI(Prefix)).


%! ontology// .
%
% ```
% ontology ::= 'Ontology:' [ ontologyIRI [ versionIRI ] ] { import }
%              { annotations } { frame }
% ```

ontology -->
  "Ontology:",
  (ontologyIRI(OntologyIri) -> (versionIRI(VersionIri) -> "" ; "") ; ""),
  'import*'(Imports),
  'annotations*'(Annotations),
  'frame*'(Frames).



ontologyIRI ::= IRI versionIRI ::= IRI import ::= 'Import:' IRI frame ::= datatypeFrame | classFrame | objectPropertyFrame | dataPropertyFrame | annotationPropertyFrame | individualFrame | misc

%! classIRI(-Iri:atom)// .
%
% ```
% classIRI ::= IRI
% ```

classIRI(Iri) -->
  'IRI'(Iri).



% comment// .
%
% Comments are maximal sequences of Unicode characters starting with a
% `#' and not containing a line feed or a carriage return.
%
% Note that comments are only recognized where white space is allowed,
% and thus not inside the above non-terminals.

comment -->
  "#",
  string(_),
  ("\n" ; "\r"), !.



%! 'Datatype'(-Iri:atom)// .
%
% ```
% Datatype ::= datatypeIRI | 'integer' | 'decimal' | 'float' | 'string'
% ```

'Datatype'(Iri)         --> !, datatypeIRI(Iri).
'Datatype'(xsd:integer) --> !, "integer".
'Datatype'(xsd:decimal) --> !, "decimal".
'Datatype'(xsd:float)   --> !, "float".
'Datatype'(xsd:string)  --> "string".



%! datatypeIRI(-Iri:atom)// .
%
% ```
% datatypeIRI ::= IRI
% ```

datatypeIRI(Iri) -->
  'IRI'(Iri).



%! dataPropertyIRI(-Iri:atom)// .
%
% ```
% dataPropertyIRI ::= IRI
% ```

dataPropertyIRI(Iri) -->
  'IRI'(Iri).



%! decimalLiteral(-Literal:rdf_literal)// .
%
% ```
% decimalLiteral ::= ['+' | '-'] digits '.' digits
% ```

decimalLiteral(literal(type(xsd:decimal,Rational))) -->
  ("+" -> {Sign = 1} ; "-" -> {Sign = -1} ; {Sign = 1}),
  'digit+'(Weights1),
  ".",
  'digit+'(Weights2),
  {
    integer_weights(Integer, Weights1),
    fractional_weights(Fractional, Weights2),
    decimal_parts(UnsignedRational, Integer, Fractional),
    Rational is Sign * UnsignedRational
  }.



%! entity(-Iri:atom)//
%
% ```
% entity ::= 'Datatype' '(' Datatype ')'
%          | 'Class' '(' classIRI ')'
%          | 'ObjectProperty' '(' objectPropertyIRI ')'
%          | 'DataProperty' '('dataPropertyIRI ')'
%          | 'AnnotationProperty' '(' annotationPropertyIRI ')'
%          | 'NamedIndividual' '(' individualIRI ')'
% ```

entity(Iri) --> "Datatype(",           !, 'Datatype'(Iri),            ")".
entity(Iri) --> "Class(",              !, classIRI(Iri),              ")".
entity(Iri) --> "ObjectProperty(",     !, objectPropertyIRI(Iri),     ")".
entity(Iri) --> "DataProperty(",       !, dataPropertyIRI(Iri),       ")".
entity(Iri) --> "AnnotationProperty(", !, annotationPropertyIRI(Iri), ")".
entity(Iri) --> "NamedIndividual(",    individualIRI(Iri),         ")".		  



%! exponent(-Exponent:integer)// .
%
% ```
% exponent ::= ('e' | 'E') ['+' | '-'] digits
% ```

exponent(Exponent) -->
  ("e" ; "E"),
  ("+" -> {Sign = 1} ; "-" -> {Sign = -1}),
  'digit_weight+'(Weights),
  {
    integer_weights(Integer, Weights),
    Exponent is Sign * 10 ^ Integer
  }.



%! floatingPointLiteral(-Literal:rdf_literal)// .
%
% ```
% floatingPointLiteral ::= [ '+' | '-']
%                          ( digits ['.'digits] [exponent] | '.' digits[exponent])
%                          ( 'f' | 'F' )
% ```

floatingPointLiteral(literal(xsd:float,Float))) -->
  ("+" -> {Sign = 1} ; "-" -> {Sign = -1} ; {Sign = 1}),
  (   'digit_weight+'(Weights1)
  ->  ("." -> 'digit_weight+'(Weights2) ; {Weights2 = []})
  ;   {Weights1 = []},
      ".",
      'digit_weight+'(Weights2)
  ),
  (exponent(Exponent) -> "" ; {Exponent = 0}),
  ("f" ; "F"),
  {
    integer_weights(Integer, Weights1),
    fractional_weights(Fractional, Weights2),
    Float is Sign * float(Integer + Fractional) * Exponent
  }.



%! fullIRI(-Iri:atom)// .
%
% ```
% fullIRI := an IRI as defined in [RFC 3987], enclosed in a pair of
%            < (U+3C) and > (U+3E) characters
% ```

fullIRI(Iri) -->
  'IRI'(Iri).



%! individual(-Individual:atom))// .
%
% ```
% individual ::= individualIRI | nodeID
% ```

individual(S) --> individualIRI(S), !.
individual(S) --> nodeID(S).



%! inividualIRI(-Iri:atom)//.
%
% ```
% individualIRI ::= IRI
% ```

individualIRI(Iri) -->
  'IRI'(Iri).



%! 'IRI'(-Iri:atom)// .
%
% ```
% IRI := fullIRI | abbreviatedIRI | simpleIRI
% ```

'IRI'(Iri) --> fullIRI(Iri), !.
'IRI'(Iri) --> abbreviatedIRI(Iri), !.
'IRI'(Iri) --> simpleIRI(Iri).



%! integerLiteral(-Literal:rdf_literal)// .
%
% ```
% integerLiteral ::= [ '+' | '-' ] digits
% ```

integerLiteral(literal(type(xsd:integer,Integer))) -->
  ("+" -> {Sign = 1} ; "-" -> {Sign = -1}),
  digits(Weights),
  {
    integer_weights(UnsignedInteger, Weights),
    Integer is Sign * UnsignedInteger
  }.



%! langageTag(-LanguageTag:atom)// .
%
% ```
% languageTag := @ (U+40) followed a nonempty sequence of characters
%                matching the langtag production from [BCP 47]
% ```

languageTag(LTag) -->
  "@",
  'Language-Tag'(LTag).



%! lexicalValue(-Lex:atom)// .
%
% ```
% lexicalValue ::= quotedString
% ```

lexicalValue(Lex) -->
  quotedString(Lex).



%! literal(-Literal:rdf_literal)// .
%
% ```
% literal ::= typedLiteral
%           | stringLiteralNoLanguage
%           | stringLiteralWithLanguage
%           | integerLiteral
%           | decimalLiteral
%           | floatingPointLiteral
% ```

literal(Lit) --> typedLiteral(Lit), !,.
literal(Lit) --> stringLiteralNoLanguage(Lit), !.
literal(Lit) --> stringLiteralWithLanguage(Lit), !.
literal(Lit) --> integerLiteral(Lit), !.
literal(Lit) --> decimalLiteral(Lit), !.
literal(Lit) --> floatingPointLiteral(Lit).



%! nodeID(-BNodeLabel:atom)// .
%
% ```
% nodeID := a finite sequence of characters matching the BLANK_NODE_LABEL
%           production of [SPARQL]
% ```

nodeID(BNodeLabel) -->
  dcg_atom('BLANK_NODE_LABEL', BNodeLabel).



%! nonNegativeInteger(-Literal:rdf_literal)// .
%
% ```
% nonNegativeInteger ::= zero | positiveInteger
% ```

nonNegativeInteger(literal(type(xsd:nonNegativeInteger,0))) -->
  "0", !.
nonNegativeInteger(Literal) -->
  positiveInteger(Literal).



%! objectPropertyIRI(-Iri:atom)// .
%
% ```
% objectPropertyIRI ::= IRI
% ```

objectPropertyIRI(Iri) -->
  'IRI'(Iri).



%! positiveInteger(-Literal:rdf_litera;)// .
%
% ```
% positiveInteger ::= nonZero { digit }
% ```

positiveInteger(literal(type(xsd:positiveInteger,Integer))) -->
  'digit_weight+'([H|T]),
  {
    H =\= 0,
    integer_weights(Integer, [H|T])
  }.



%! prefixName(-Prefix:atom)// .
%
% ```
% prefixName := a finite sequence of characters matching the PNAME_NS
%               production of [SPARQL] and not matching any of the keyword
%               terminals of the syntax
% ```
%
% Prefixes in abbreviated IRIs must not match any of the keywords of
% this syntax.
%
% Prefixes should begin with lower case letters so that they do not
% clash with colon-terminated keywords introduced in future versions
% of this syntax.

prefixName(Prefix) -->
  % Check whether the first character is a lower case letter.
  dcg_peek_code(Code),
  {between(0'a, 0'z, Code)},
  dcg_string('PNAME_NS', Prefix),
  % Check whether the prefix is not a keyword.
  {throw_if_manchester_keyword(Prefix)}.



%! quotedString(-Lex:atom)// .
%
% ```
% quotedString := a finite sequence of characters in which
%                 " (U+22) and \ (U+5C) occur only in pairs
%                 of the form \" (U+5C, U+22) and \\ (U+5C, U+5C),
%                 enclosed in a pair of " (U+22) characters
% ```

quotedString(Lex) -->
  "\"", !,
  dcg_atom(quoted_string_codes_, Lex),
  "\"".

quoted_string_codes_([0'\\,0'\"|T]) -->
  "\\\"", !,
  quoted_string_codes(T).
quoted_string_codes_([0'\",0'\"|T]) -->
  "\"\"", !,
  quoted_string_codes(T).
quoted_string_codes_([H|T]) -->
  [H],
  {\+ memberchk(H, [0'\\,0'\"])},
  quoted_string_codes(T).



%! simpleIRI(-SimpleIri:atom)// .
%
% ```
% simpleIRI := a finite sequence of characters matching the PN_LOCAL
%              production of [SPARQL] and not matching any of the keyword
%              terminals of the syntax
% ```
%
% Local parts with no prefix are expanded as if they had an initial colon
% and must not match any keyword of this syntax.

simpleIRI(Iri) -->
  dcg_string('PN_LOCAL', LocalName),
  {
    throw_if_manchester_keyword(LocalName),
    rdf_prefix_iri('':LocalName, Iri)
  }.


%! stringLiteralNoLanguage(-Literal:rdf_literal)// .
%
% ```
% stringLiteralNoLanguage ::= quotedString
% ```

stringLiteralNoLanguage(literal(type(xsd:string,Lex))) -->
  quotedString(Lex).



%! stringLiteralWithLanguage(-Literal:rdf_literal)// .
%
% ```
% stringLiteralWithLanguage ::= quotedString languageTag
% ```

stringLiteralWithLanguage(literal(lang(LTag,Lex))) -->
  quotedString(Lex),
  languageTag(LTag).



%! typedLiteral(-Literal:rdf_literal)// .
%
% ```
% typedLiteral ::= lexicalValue '^^' Datatype
% ```

typedLiteral(literal(type(D,Lex))) -->
  lexicalValue(Lex),
  "^^",
  'Datatype'(D).





% HELPERS %

throw_if_manchester_keyword(Keyword) :-
  manchester_keyword(Keyword), !,
  syntax_error(grammar(manchester,keyword,Keyword)).
throw_if_manchester_keyword(_).

manchester_keyword('AnnotationProperty').
manchester_keyword('Annotations').
manchester_keyword('Asymmetric').
manchester_keyword('Characteristics').
manchester_keyword('Class').
manchester_keyword('DataProperty').
manchester_keyword('Datatype').
manchester_keyword('DifferentFrom').
manchester_keyword('DifferentIndividuals').
manchester_keyword('DisjointClasses').
manchester_keyword('DisjointProperties').
manchester_keyword('DisjointUnionOf').
manchester_keyword('DisjointWith').
manchester_keyword('Domain').
manchester_keyword('EquivalentProperties').
manchester_keyword('EquivalentTo').
manchester_keyword('Facts').
manchester_keyword('Functional').
manchester_keyword('HasKey').
manchester_keyword('Import').
manchester_keyword('Individual').
manchester_keyword('InverseFunctional').
manchester_keyword('Irreflexive').
manchester_keyword('ObjectProperty').
manchester_keyword('Ontology').
manchester_keyword('Prefix').
manchester_keyword('Range').
manchester_keyword('Reflexive').
manchester_keyword('SameAs').
manchester_keyword('SameIndividual').
manchester_keyword('SubClassOf').
manchester_keyword('SubPropertyChain').
manchester_keyword('SubPropertyOf').
manchester_keyword('Symmetric').
manchester_keyword('Transitive').
manchester_keyword('Types').
