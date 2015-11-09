:- module(
  manchester,
  [
    abbreviatedIRI//1, % ?Iri:atom
    annotationPropertyIRI//1, % ?Iri:atom
    classIRI//1, % ?Iri:atom
    'Datatype'//1, % ?Iri:atom
    datatypeIRI//1, % ?Iri:atom
    dataPropertyIRI//1, % ?Iri:atom
    decimalLiteral//1, % ?Literal:compound
    digit//1, % ?Digit:between(0,9)
    digits//1, % ?N:nonneg
    entity//1, % ?Iri:atom
    exponent//1, % ?Exponent:integer
    floatingPointLiteral//1, % ?Literal:compound
    fullIRI//1, % ?Iri:atom
    individual//1, % ?Individual:atom
    individualIRI//1, % ?Individual:atom
    'IRI'//1, % ?Iri:atom
    integerLiteral//1, % ?Literal:compound
    languageTag//1, % ?LanguageTag:list(string)
    literal//1, % ?Literal:compound
    nodeID//1, % ?BlankNodeLabel:atom
    nonNegativeInteger//1, % ?N:nonneg
    nonZero//1, % ?Digit:between(0,9)
    objectPropertyIRI//1, % ?Iri:atom
    positiveInteger//1, % ?N:positive_integer
    prefixName//1, % ?Prefix:string
    quotedString//1, % ?String:string
    simpleIRI//1, % ?SimpleIri:string
    stringLiteralNoLanguage//1, % ?Literal:compound
    stringLiteralWithLanguage//1, % ?Literal:compound
    typedLiteral//1, % ?Literal:compound
    zero//1 % ?Digit:between(0,0)
  ]
).

/** <module> OWL 2 Web Ontology Language: Manchester Syntax (Second Edition)

```
'List'(NT) --> NT, list(NT).
list(NT) --> ",", NT, list(NT).
list(_) --> "".

'2List'(NT) --> NT, ",", 'List'(NT).

'AnnotatedList'(NT) --> ?(annotations), NT, annotated_list(NT).
annotated_list(NT) --> ",", ?(annotations), NT, annotated_list(NT).
annotated_list(_) --> "".
```

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(iri/rfc3987)).
:- use_module(library(ltag/rfc5646)).
:- use_module(library(math/positional)).
:- use_module(library(math/rational_ext)).
:- use_module(library(sparql/sparql10_code)).





%! abbreviatedIRI(?AbbreviatedIri:string)// .
% ```
% abbreviatedIRI := a finite sequence of characters matching the PNAME_LN
%                   production of [SPARQL]
% ```

abbreviatedIRI(S) --> dcg_string('PNAME_LN', S).



%! annotationPropertyIri(?Iri:atom)// .
% ```
% annotationPropertyIRI ::= IRI
% ```

annotationPropertyIRI(Iri) --> 'IRI'(Iri).



%! classIRI(?Iri:atom)// .
% ```
% classIRI ::= IRI
% ```

classIRI(Iri) --> 'IRI'(Iri).



%! Datatype(?Iri:atom)// .
% ```
% Datatype ::= datatypeIRI | 'integer' | 'decimal' | 'float' | 'string'
% ```

'Datatype'(Iri) --> datatypeIRI(Iri).
'Datatype'(xsd:integer) --> "integer".
'Datatype'(xsd:decimal) --> "decimal".
'Datatype'(xsd:float) --> "float".
'Datatype'(xsd:string) --> "string".



%! datatypeIRI(?Iri:atom)// .
% ```
% datatypeIRI ::= IRI
% ```

datatypeIRI(Iri) --> 'IRI'(Iri).



%! dataPropertyIRI(?Iri:atom)// .
% ```
% dataPropertyIRI ::= IRI
% ```

dataPropertyIRI(Iri) --> 'IRI'(Iri).



%! decimalLiteral(?Literal:compound)// .
% ```
% decimalLiteral ::= ['+' | '-'] digits '.' digits
% ```

decimalLiteral(literal(type(xsd:decimal,N))) -->
  ("+", {Sg = 1} ; "-", {Sg = -1}),
  digits(N1),
  ".",
  digits(N2),
  {
    rational_parts_weights(N0, N1, N2),
    N is Sg * N0
  }.



%! digit(?Digit:between(0,9))// .
% ```
% digit ::= zero | nonZero
% ```

digit(D) --> zero(D).
digit(D) --> nonZero(D).



%! digits(?N:nonneg)// .
% ```
% digits ::= digit { digit }
% ```

digits(N) --> +(digit, N, [convert1(clpfd_positional)]).



%! entity(?Iri:atom)//
% ```
% entity ::= 'Datatype' '(' Datatype ')'
%          | 'Class' '(' classIRI ')'
%          | 'ObjectProperty' '(' objectPropertyIRI ')'
%          | 'DataProperty' '('dataPropertyIRI ')'
%          | 'AnnotationProperty' '(' annotationPropertyIRI ')'
%          | 'NamedIndividual' '(' individualIRI ')'
% ```

entity(Iri) --> "Datatype(", 'Datatype'(Iri), ")".
entity(Iri) --> "Class(", classIRI(Iri), ")".
entity(Iri) --> "ObjectProperty(", objectPropertyIRI(Iri), ")".
entity(Iri) --> "DataProperty(", dataPropertyIRI(Iri), ")".
entity(Iri) --> "AnnotationProperty(", annotationPropertyIRI(Iri), ")".
entity(Iri) --> "NamedIndividual(", individualIRI(Iri), ")".		  



%! exponent(?Exponent:integer)// .
% ```
% exponent ::= ('e' | 'E') ['+' | '-'] digits
% ```

exponent(Exp) -->
  ("e" ; "E"),
  ("+", {Sg = 1} ; "-", {Sg = -1}),
  digits(N0),
  {Exp is Sg * N0}.



%! floatingPointLiteral(?N:float)// .
% ```
% floatingPointLiteral ::= [ '+' | '-']
%                          ( digits ['.'digits] [exponent] | '.' digits[exponent])
%                          ( 'f' | 'F' )
% ```

floatingPointLiteral(N) -->
  ("+", {Sg = 1} ; "-", {Sg = -1}),
  (   digits(N1), (".", digits(N2) ; {N2 = 0})
  ;   {N1 = 0}, ".", digits(N2)
  ),
  (exponent(Exp) ; {Exp = 0}),
  ("f" ; "F"),
  {
    rational_parts_weights(N0, N1, N2),
    N is Sg * N0 * 10 ^ Exp
  }.



%! fullIRI(?Iri:atom)// .
% ```
% fullIRI := an IRI as defined in [RFC 3987], enclosed in a pair of
%            < (U+3C) and > (U+3E) characters
% ```

fullIRI(Iri) --> 'IRI'(Iri).



%! individual(?Individual:atom))// .
% ```
% individual ::= individualIRI | nodeID
% ```

individual(S) --> individualIRI(S).
individual(S) --> nodeID(S).



%! inividualIRI(?Iri:atom)//.
% ```
% individualIRI ::= IRI
% ```

individualIRI(Iri) --> 'IRI'(Iri).



%! 'IRI'(?Iri:atom)// .
% ```
% IRI := fullIRI | abbreviatedIRI | simpleIRI
% ```

'IRI'(Iri) --> fullIRI(Iri).
'IRI'(Iri) --> abbreviatedIRI(Iri).
'IRI'(Iri) --> simpleIRI(Iri).



%! integerLiteral(?Literal:compound)// .
% ```
% integerLiteral ::= ['+' | '-'] digits
% ```

integerLiteral(literal(type(xsd:integer,N))) -->
  ("+", {Sg = 1} ; "-", {Sg = -1}),
  digits(N0),
  {N is Sg * N0}.



%! langageTag(?LanguageTag:list(string))// .
% ```
% languageTag := @ (U+40) followed a nonempty sequence of characters
%                matching the langtag production from [BCP 47]
% ```

languageTag(LTag) --> "@", 'Language-Tag'(LTag).



%! lexicalValue(?LexicalValue:atom)// .
% ```
% lexicalValue ::= quotedString
% ```

lexicalValue(Lex) --> quotedString(Lex).



%! literal(?Literal:compound)// .
% ```
% literal ::= typedLiteral
%           | stringLiteralNoLanguage
%           | stringLiteralWithLanguage
%           | integerLiteral
%           | decimalLiteral
%           | floatingPointLiteral
% ```

literal(Lit) --> typedLiteral(Lit).
literal(Lit) --> stringLiteralNoLanguage(Lit).
literal(Lit) --> stringLiteralWithLanguage(Lit).
literal(Lit) --> integerLiteral(Lit).
literal(Lit) --> decimalLiteral(Lit).
literal(Lit) --> floatingPointLiteral(Lit).



%! nodeID(?BlankNodeLabel:atom)// .
% ```
% nodeID := a finite sequence of characters matching the BLANK_NODE_LABEL
%           production of [SPARQL]
% ```

nodeID(BlankNodeLabel) --> dcg_atom('BLANK_NODE_LABEL', BlankNodeLabel).



%! nonNegativeInteger(?N:nonneg)// .
% ```
% nonNegativeInteger ::= zero | positiveInteger
% ```

nonNegativeInteger(N) --> zero(N).
nonNegativeInteger(N) --> positiveInteger(N).



%! nonZero(?Digit:between(0,9))// .
%! nonZero(?Digit:between(0,9), ?Code:code)// .
% ```
% nonZero := '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
% ```

nonZero(D) --> nonZero(D, _).
nonZero(1, 0'1) --> "1".
nonZero(2, 0'2) --> "2".
nonZero(3, 0'3) --> "3".
nonZero(4, 0'4) --> "4".
nonZero(5, 0'5) --> "5".
nonZero(6, 0'6) --> "6".
nonZero(7, 0'7) --> "7".
nonZero(8, 0'8) --> "8".
nonZero(9, 0'9) --> "9".



%! objectPropertyIRI(?Iri:atom)// .
% ```
% objectPropertyIRI ::= IRI
% ```

objectPropertyIRI(Iri) --> 'IRI'(Iri).



%! positiveInteger(?N:positiveInteger)// .
% ```
% positiveInteger ::= nonZero { digit }
% ```

positiveInteger(N) -->
  {clpfd_positional([H|T], N)},
  nonZero(H),
  *(digit, T, []).



%! prefixName(?Prefix:string)// .
% ```
% prefixName := a finite sequence of characters matching the PNAME_NS
%               production of [SPARQL] and not matching any of the keyword
%               terminals of the syntax
% ```

prefixName(Prefix) --> dcg_string('PNAME_NS', Prefix).



%! quotedString(?String:string)// .
% ```
% quotedString := a finite sequence of characters in which
%                 " (U+22) and \ (U+5C) occur only in pairs
%                 of the form \" (U+5C, U+22) and \\ (U+5C, U+5C),
%                 enclosed in a pair of " (U+22) characters
% ```

quotedString(S) --> "\"", dcg_string(quoted_string_codes1, S), "\"".
quoted_string_codes1([0'\\,0'\"|T]) --> "\\\"", !, quoted_string_codes1(T).
quoted_string_codes1([0'\",0'\"|T]) --> "\"\"", !, quoted_string_codes1(T).
quoted_string_codes1([H|T]) -->
  [H], {H \== 0'\\, H \== 0'\"}, !,
  quoted_string_codes1(T).



%! simpleIRI(?SimpleIri:string)// .
% ```
% simpleIRI := a finite sequence of characters matching the PN_LOCAL
%              production of [SPARQL] and not matching any of the keyword
%              terminals of the syntax
% ```

simpleIRI(S) --> dcg_string('PN_LOCAL', S).



%! stringLiteralNoLanguage(?Literal:compound)// .
% ```
% stringLiteralNoLanguage ::= quotedString
% ```

stringLiteralNoLanguage(literal(type(xsd:string,Lex))) --> quotedString(Lex).



%! stringLiteralWithLanguage// .
% ```
% stringLiteralWithLanguage ::= quotedString languageTag
% ```

stringLiteralWithLanguage(literal(lang(LTag,Lex))) -->
  quotedString(Lex),
  languageTag(LTag).



%! typedLiteral(?Literal:compound)// .
% ```
% typedLiteral ::= lexicalValue '^^' Datatype
% ```

typedLiteral(literal(type(D,Lex))) --> lexicalValue(Lex), "^^", 'Datatype'(D).



%! zero(?Weight:between(0,0))// .
%! zero(?Weight:between(0,0), ?Code:code)// .
% ```
% zero ::= '0'
% ```

zero(W) --> (W, _).
zero(0, 0'0) --> "0".
