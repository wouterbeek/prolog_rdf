:- module(
  sw_iri,
  [
    annotationPropertyIRI//1, % ?Iri:atom
    classIRI//1, % ?Iri:atom
    dataPropertyIRI//1, % ?Iri:atom
    'Datatype'//1, % ?Iri:atom
    datatypeIRI//1, % ?Iri:atom
    individualIRI//1, % ?Iri:atom
    iri//1, % ?Iri:atom
    'IRI'//1, % ?Iri:atom
    'IRIREF'//1, % ?Iri:atom
    objectPropertyIRI//1, % ?Iri:atom
    'PNAME_NS'//1 % ?Prefix:atom
  ]
).

/** <module> IRI definitions in Semantic Web grammars

Various grammar rules for the use of IRIs in SW standards.

Most standards allow IRIs to be abbreviated by splitting them in
 an IRI prefix and a local name.

---

@author Wouter Beek
@compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
@compat SPARQL 1.1 Query
@compat Turtle 1.1
@version 2015/08, 2015/11
*/

:- use_module(library(char_ext)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/sw_char)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(dif)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(iri/rfc3987)).

:- rdf_meta('Datatype'(r,?,?)).





%! abbreviatedIRI(?PrefixedIri:compound)// .
% ```bnf
% abbreviatedIRI := a finite sequence of characters matching the PNAME_LN
%                   production of [SPARQL]
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

abbreviatedIRI(PrefixedIri) --> 'PNAME_LN'(PrefixedIri).



%! annotationPropertyIRI(?Iri:atom)// .
% ```bnf
% annotationPropertyIRI ::= IRI
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

annotationPropertyIRI(Iri) --> 'IRI'(Iri).



%! classIRI(?Iri:atom)// .
% ```bnf
% classIRI ::= IRI
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

classIRI(Iri) --> 'IRI'(Iri).



%! dataPropertyIRI(?Iri:atom)// .
% ```bnf
% dataPropertyIRI ::= IRI
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

dataPropertyIRI(Iri) --> 'IRI'(Iri).



%! 'Datatype'(?Iri:atom)// .
% ```bnf
% Datatype ::= datatypeIRI | 'integer' | 'decimal' | 'float' | 'string'
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

'Datatype'(Iri)         --> datatypeIRI(Iri).
'Datatype'(xsd:integer) --> "integer".
'Datatype'(xsd:decimal) --> "decimal".
'Datatype'(xsd:float)   --> "float".
'Datatype'(xsd:string)  --> "string".



%! datatypeIRI(?Iri:atom)// .
% ```bnf
% datatypeIRI ::= IRI
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

datatypeIRI(Iri) --> 'IRI'(Iri).



%! fullIRI(?Iri:atom)// .
% ```bnf
% fullIRI := an IRI as defined in [RFC 3987], enclosed in a pair of
%            < (U+3C) and > (U+3E) characters
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

fullIRI(Iri) --> "<", 'IRI'(Iri), ">".



%! individualIRI(?Iri:atom)// .
% ```bnf
% individualIRI ::= IRI
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

individualIRI(Iri) --> 'IRI'(Iri).



%! iri(?Iri:or([atom,compound]))// is det.
%
% ```bnf
% iri ::= IRIREF | PrefixedName
% ```
%
% @compat SPARQL 1.0 [135]
% @compat SPARQL 1.1 Query [136]
% @compat Turtle 1.1 [135a]

iri(Iri)         --> 'IRIREF'(Iri).
iri(PrefixedIri) --> 'PrefixedName'(PrefixedIri).



%! 'IRI'(?Iri:atom)// .
% ```bnf
% IRI := fullIRI | abbreviatedIRI | simpleIRI
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

'IRI'(Iri) --> fullIRI(Iri).
'IRI'(Iri) --> abbreviatedIRI(Iri).
'IRI'(Iri) --> simpleIRI(Iri).



%! 'IRI_REF'(?Iri:atom)// .
% ```bnf
% IRI_REF ::=  '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% ```
%
% @compat SPARQL 1.0 [70]
% @deprecated Use 'IRIREF'//1 instead.

'IRI_REF'(Iri) --> 'IRIREF'(Iri).



%! 'IRIREF'(?Iri:atom)// .
% ```bnf
% [SPARQL]                     IRIREF ::= '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% [N-Quads,N-Triples,Turtle]   IRIREF ::= '<'
%                                         ( [^#x00-#x20<>"{}|^`\] | UCHAR )*
%                                         '>'
% ```
%
% @compat N-Quads 1.1 [10].
% @compat N-Triples 1.1 [8].
% @compat SPARQL 1.1 Query [139].
% @compat Turtle 1.1 [18].
% @tbd What about DELETE (decimal 127)?

'IRIREF'(Iri) --> "<", dcg_atom('IRIREF_codes', Iri), ">".
'IRIREF_codes'([H|T]) --> 'IRIREF_code'(H), !, 'IRIREF_codes'(T).
'IRIREF_codes'([])    --> "".
'IRIREF_code'(_) --> 'CTL', !, {fail}.
'IRIREF_code'(_) --> "<",   !, {fail}.
'IRIREF_code'(_) --> ">",   !, {fail}.
'IRIREF_code'(_) --> "\"",  !, {fail}.
'IRIREF_code'(_) --> "{",   !, {fail}.
'IRIREF_code'(_) --> "}",   !, {fail}.
'IRIREF_code'(_) --> "|",   !, {fail}.
'IRIREF_code'(_) --> "^",   !, {fail}.
'IRIREF_code'(_) --> "\`",  !, {fail}.
'IRIREF_code'(_) --> "\\",  !, {fail}.
'IRIREF_code'(C) --> 'UCHAR'(C).
'IRIREF_code'(C) --> [C].



%! 'IRIref'(?Iri:atom)// .
% ```bnf
% IRIref ::= IRI_REF | PrefixedName
% ```
%
% @compat SPARQL 1.0 [67]
% @deprecated Use iri//1 instead.

'IRIref'(Iri)         --> 'IRI_REF'(Iri).
'IRIref'(PrefixedIri) --> 'PrefixedName'(PrefixedIri).



%! objectPropertyIRI(?Iri:atom)// .
% ```bnf
% objectPropertyIRI ::= IRI
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

objectPropertyIRI(Iri) --> 'IRI'(Iri).



%! 'PN_PREFIX'(?Prefix:atom)// .
% The **prefix label** used in *prefixed names*.
%
% ```bnf
% PN_PREFIX ::= PN_CHARS_BASE ( ( PN_CHARS | '.' )* PN_CHARS )?
% ```
%
% @compat SPARQL 1.0 [167].
% @compat SPARQL 1.1 Query [168].
% @compat Turtle 1.1 [167s].
