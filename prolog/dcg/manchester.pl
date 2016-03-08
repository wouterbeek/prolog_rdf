:- module(
  manchester,
  [
    abbreviatedIRI//1, % ?Iri:atom
    annotationPropertyIRI//1, % ?Iri:atom
    classIRI//1, % ?Iri:atom
    comment//0,
    'Datatype'//1, % ?Iri:atom
    datatypeIRI//1, % ?Iri:atom
    dataPropertyIRI//1, % ?Iri:atom
    decimalLiteral//1, % ?Literal:compound
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
:- reexport(
     library(dcg/dcg_ext),
     [
       digit//1 % ?Digit:between(0,9)
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
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_peek)).
:- use_module(library(dcg/sparql10)).
:- use_module(library(ltag/rfc5646)).
:- use_module(library(math/rational_ext)).
:- use_module(library(rdf/rdf_prefix)).





%! abbreviatedIRI(?AbbreviatedIri:string)// .
% ```
% abbreviatedIRI := a finite sequence of characters matching the PNAME_LN
%                   production of [SPARQL]
% ```

abbreviatedIRI(Iri) --> 'PNAME_LN'(Iri).



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



% comment// .
% Comments are maximal sequences of Unicode characters starting with a `#`
% and not containing a line feed or a carriage return.
%
% Note that comments are only recognized where white space is allowed,
% and thus not inside the above non-terminals.
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

comment --> "#", string(_), ("\n" ; "\r"), !.



%! Datatype(?Iri:atom)// .
% ```
% Datatype ::= datatypeIRI | 'integer' | 'decimal' | 'float' | 'string'
% ```

'Datatype'(Iri)         --> datatypeIRI(Iri).
'Datatype'(xsd:integer) --> "integer".
'Datatype'(xsd:decimal) --> "decimal".
'Datatype'(xsd:float)   --> "float".
'Datatype'(xsd:string)  --> "string".



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

decimalLiteral(literal(type(xsd:decimal,Rat))) -->
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1} ; {Sg = 1}),
  digits(Ds1),
  {pos_sum(Ds1, I)},
  ".",
  digits(Ds2),
  {
    pos_frac(Ds2, Frac),
    rational_parts(Rat0, I, Frac),
    Rat is Sg * Rat0
  }.



%! digit(?Digit:between(0,9))// .
% ```
% digit ::= zero | nonZero
% ```



%! digits(?Digits:list(between(0,9)))// .
% ```
% digits ::= digit { digit }
% ```

digits(Ds) --> +(digit, Ds).



%! entity(?Iri:atom)//
% ```
% entity ::= 'Datatype' '(' Datatype ')'
%          | 'Class' '(' classIRI ')'
%          | 'ObjectProperty' '(' objectPropertyIRI ')'
%          | 'DataProperty' '('dataPropertyIRI ')'
%          | 'AnnotationProperty' '(' annotationPropertyIRI ')'
%          | 'NamedIndividual' '(' individualIRI ')'
% ```

entity(Iri) --> "Datatype(",           'Datatype'(Iri),            ")".
entity(Iri) --> "Class(",              classIRI(Iri),              ")".
entity(Iri) --> "ObjectProperty(",     objectPropertyIRI(Iri),     ")".
entity(Iri) --> "DataProperty(",       dataPropertyIRI(Iri),       ")".
entity(Iri) --> "AnnotationProperty(", annotationPropertyIRI(Iri), ")".
entity(Iri) --> "NamedIndividual(",    individualIRI(Iri),         ")".		  



%! exponent(?Exponent:integer)// .
% ```
% exponent ::= ('e' | 'E') ['+' | '-'] digits
% ```

exponent(Exp) -->
  ("e" ; "E"),
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1}),
  digits(Ds),
  {pos_sum(Ds, I), Exp is Sg * 10 ^ I}.



%! floatingPointLiteral(?N:float)// .
% ```
% floatingPointLiteral ::= [ '+' | '-']
%                          ( digits ['.'digits] [exponent] | '.' digits[exponent])
%                          ( 'f' | 'F' )
% ```

floatingPointLiteral(Rat) -->
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1} ; {Sg = 1}),
  (   digits(Ds1)
  ->  ("." -> digits(Ds2) ; {Ds2 = []})
  ;   {Ds1 = []},
      ".",
      digits(Ds2)
  ),
  def(exponent, Exp, 0),
  ("f" ; "F"), !,
  {
    pos_sum(Ds1, I),
    pos_frac(Ds2, Frac),
    Rat is Sg * float(I + Frac) * Exp
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
% integerLiteral ::= [ '+' | '-' ] digits
% ```

integerLiteral(literal(type(xsd:integer,I))) -->
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1}),
  digits(Ds),
  {pos_sum(Ds, I0), I is Sg * I0}.



%! langageTag(?LanguageTag:list(string))// .
% ```
% languageTag := @ (U+40) followed a nonempty sequence of characters
%                matching the langtag production from [BCP 47]
% ```

languageTag(LTag) -->
  "@", 'Language-Tag'(LTag0),
  {maplist(atom_string, LTag, LTag0)}.



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

nonNegativeInteger(N) --> zero(N), !.
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

positiveInteger(I) --> nonZero(H), *(digit, T), {pos_sum([H|T], I)}.



%! prefixName(?Prefix:atom)// .
% ```
% prefixName := a finite sequence of characters matching the PNAME_NS
%               production of [SPARQL] and not matching any of the keyword
%               terminals of the syntax
% ```
%
% Prefixes in abbreviated IRIs must not match any of the keywords of
% this syntax.
%
% Prefixes should begin with lower case letters so that they do not clash
% with colon-terminated keywords introduced in future versions of this syntax.

prefixName(Prefix) -->
  dcg_peek_code(C), {between(0'a, 0'z, C)},
  dcg_string('PNAME_NS', Prefix),
  {throw_if_manchester_keyword(Prefix)}.



%! quotedString(?String:atom)// .
% ```
% quotedString := a finite sequence of characters in which
%                 " (U+22) and \ (U+5C) occur only in pairs
%                 of the form \" (U+5C, U+22) and \\ (U+5C, U+5C),
%                 enclosed in a pair of " (U+22) characters
% ```

quotedString(A) --> "\"", dcg_atom(quoted_string_codes, A), "\"".
quoted_string_codes([0'\\,0'\"|T]) --> "\\\"", !, quoted_string_codes(T).
quoted_string_codes([0'\",0'\"|T]) --> "\"\"", !, quoted_string_codes(T).
quoted_string_codes([H|T]) -->
  [H], {H \== 0'\\, H \== 0'\"}, !,
  quoted_string_codes(T).



%! simpleIRI(?SimpleIri:string)// .
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
  {throw_if_manchester_keyword(LocalName), rdf_global_id('':LocalName, Iri)}.


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





% HELPERS %

throw_if_manchester_keyword(S) :-
  manchester_keyword(S), !,
  syntax_error(manchester_keyword(S)).
throw_if_manchester_keyword(_).

manchester_keyword("AnnotationProperty").
manchester_keyword("Annotations").
manchester_keyword("Asymmetric").
manchester_keyword("Characteristics").
manchester_keyword("Class").
manchester_keyword("DataProperty").
manchester_keyword("Datatype").
manchester_keyword("DifferentFrom").
manchester_keyword("DifferentIndividuals").
manchester_keyword("DisjointClasses").
manchester_keyword("DisjointProperties").
manchester_keyword("DisjointUnionOf").
manchester_keyword("DisjointWith").
manchester_keyword("Domain").
manchester_keyword("EquivalentProperties").
manchester_keyword("EquivalentTo").
manchester_keyword("Facts").
manchester_keyword("Functional").
manchester_keyword("HasKey").
manchester_keyword("Import").
manchester_keyword("Individual").
manchester_keyword("InverseFunctional").
manchester_keyword("Irreflexive").
manchester_keyword("ObjectProperty").
manchester_keyword("Ontology").
manchester_keyword("Prefix").
manchester_keyword("Range").
manchester_keyword("Reflexive").
manchester_keyword("SameAs").
manchester_keyword("SameIndividual").
manchester_keyword("SubClassOf").
manchester_keyword("SubPropertyChain").
manchester_keyword("SubPropertyOf").
manchester_keyword("Symmetric").
manchester_keyword("Transitive").
manchester_keyword("Types").
