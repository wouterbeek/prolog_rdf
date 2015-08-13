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
@version 2015/08
*/

:- use_module(library(char_ext)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc3987), ['IRI'//1 as 'IRI_rfc3987']).
:- use_module(library(dcg/sw_char)).
:- use_module(library(dif)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta('Datatype'(r,?,?)).





%! abbreviatedIRI(?PrefixedIri:compound)// .
% ```bnf
% abbreviatedIRI := a finite sequence of characters matching the PNAME_LN
%                   production of [SPARQL]
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

abbreviatedIRI(PrefixedIri) -->
  'PNAME_LN'(PrefixedIri).



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

'Datatype'(Iri) --> datatypeIRI(Iri).
'Datatype'(xsd:integer) --> "integer".
'Datatype'(xsd:decimal) --> "decimal".
'Datatype'(xsd:float) --> "float".
'Datatype'(xsd:string) --> "string".



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

fullIRI(Iri) -->
  bracketed(angular, 'IRI_rfc3987'(Iri)).



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

iri(Iri) --> 'IRIREF'(Iri).
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

'IRIREF'(Iri) -->
  bracketed(angular, dcg_atom('IRIREF_codes', Iri)).

'IRIREF_codes'([H|T]) --> 'IRIREF_code'(H), 'IRIREF_codes'(T).
'IRIREF_codes'([]) --> [].

'IRIREF_code'(_) --> control, !, {fail}.
'IRIREF_code'(_) --> angular_bracket, !, {fail}.
'IRIREF_code'(_) --> "\"", !, {fail}.
'IRIREF_code'(_) --> curly_bracket, !, {fail}.
'IRIREF_code'(_) --> "|", !, {fail}.
'IRIREF_code'(_) --> "^", !, {fail}.
'IRIREF_code'(_) --> "\`", !, {fail}.
'IRIREF_code'(_) --> "\\", !, {fail}.
'IRIREF_code'(C) --> 'UCHAR'(C).
'IRIREF_code'(C) --> [C].



%! 'IRIref'(?Iri:atom)// .
% ```bnf
% IRIref ::= IRI_REF | PrefixedName
% ```
%
% @compat SPARQL 1.0 [67]
% @deprecated Use iri//1 instead.

'IRIref'(Iri) --> 'IRI_REF'(Iri).
'IRIref'(PrefixedIri) --> 'PrefixedName'(PrefixedIri).



%! objectPropertyIRI(?Iri:atom)// .
% ```bnf
% objectPropertyIRI ::= IRI
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

objectPropertyIRI(Iri) --> 'IRI'(Iri).



%! 'PN_LOCAL'(?LocalPart:atom)// is det.
% The **local part** of a prefixed name.
%
% ```bnf
% PN_LOCAL ::= ( PN_CHARS_U | ':' | [0-9] | PLX )
%              (
%                ( PN_CHARS | '.' | ':' | PLX )*
%                ( PN_CHARS | ':' | PLX )
%              )?
% ```
%
% @compat SPARQL 1.0 [168].
% @compat SPARQL 1.1 Query [169].
% @compat Turtle 1.1 [168s].

'PN_LOCAL'(LocalPart) --> dcg_atom('PN_LOCAL_code', LocalPart).

'PN_LOCAL_code'([H|T]) -->
  'PN_LOCAL_1'(H),
  (   '*'('PN_LOCAL_2',T0, []),
      'PN_LOCAL_3'(Last),
      {append(T0, [Last], T)}
  ;   {T = []}
  ).

'PN_LOCAL_1'(C) --> 'PN_CHARS_U'(sparql, C).
'PN_LOCAL_1'(C) --> colon(C).
'PN_LOCAL_1'(C) --> decimal_digit(C).
'PN_LOCAL_1'(C) --> 'PLX'(C).

'PN_LOCAL_2'(C) --> 'PN_CHARS_U'(sparql, C).
'PN_LOCAL_2'(C) --> dot(C).
'PN_LOCAL_2'(C) --> semi_colon(C).
'PN_LOCAL_2'(C) --> 'PLX'(C).

'PN_LOCAL_3'(C) --> 'PN_CHARS'(sparql, C).
'PN_LOCAL_3'(C) --> colon(C).
'PN_LOCAL_3'(C) --> 'PLX'(C).



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

'PN_PREFIX'(Prefix) -->
  dcg_atom('PN_PREFIX_codes', Prefix).

'PN_PREFIX_codes'([H|T]) -->
  'PN_CHARS_BASE'(H),
  (   'PN_PREFIX_codes_middle*'(T0),
      'PN_CHARS'(sparql, Last),
      {append(T0, [Last], T)}
  ;   {T = []}
  ).

'PN_PREFIX_codes_middle*'([H|T]) -->
	('PN_CHARS'(sparql, H) ; dot(H)),
  'PN_PREFIX_codes_middle*'(T).
'PN_PREFIX_codes_middle*'([]) --> [].



%! 'PNAME_LN'(?PrefixedIri:compound)// .
% ```bnf
% PNAME_LN ::= PNAME_NS PN_LOCAL
% ```
%
% @compat SPARQL 1.0 [72].
% @compat SPARQL 1.1 Query [141].
% @compat Turtle 1.1 [140s].

'PNAME_LN'(PrefixedIri) -->
  {var(PrefixedIri)}, !,
  'PNAME_NS'(Prefix),
  'PN_LOCAL'(Local),
  {PrefixedIri = Prefix:Local}.
'PNAME_LN'(Prefix:Local) -->
  'PNAME_NS'(Prefix),
  'PN_LOCAL'(Local).



%! 'PNAME_NS'(?Prefix:atom)// is det.
% An IRI prefix.
%
% Notice that the empty string is also a prefix label.
%
% ```bnf
% PNAME_NS ::= PN_PREFIX? ':'
% ```
%
% @compat SPARQL 1.0 [71].
% @compat SPARQL 1.1 Query [140].
% @compat Turtle 1.1 [139s].

'PNAME_NS'(Prefix) --> 'PN_PREFIX'(Prefix), ":".
'PNAME_NS'('') --> ":".



%! prefixID(?PrefixLabel:atom, ?Iri:atom)// .
% ```ebnf
% prefixID ::= '@prefix' PNAME_NS IRIREF '.'
% ```
%
% @compat Turtle 1.1 [4].

prefixID(PrefixLabel, Iri) -->
  "@prefix",
  'PNAME_NS'(PrefixLabel),
  'IRIREF'(Iri),
  ".",
  {rdf_register_prefix(PrefixLabel, Iri)}.



%! prefixName(?Iri:atom)// .
% # Syntax
%
% ```bnf
% prefixName := a finite sequence of characters matching the PNAME_NS
%               production of [SPARQL] and not matching any of the keyword
%               terminals of the syntax
% ```
%
% ## Current keywords
%
% Prefixes in abbreviated IRIs must not match any of the keywords of
%  this syntax.
%
% ## Future keywords
%
% Prefixes should begin with lower case letters so that they do not clash
% with colon-terminated keywords introduced in future versions of this syntax.
%
% ---
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

prefixName -->
  'PNAME_NS'(Prefix),
  {
    (   oms_current_keyword(Prefix)
    ->  syntax_error(oms_current_keyword(Prefix))
    ;   true
    ),
    (   first_char(Prefix, FirstChar),
        char_type(FirstChar, lower)
    ->  true
    ;   syntax_error(oms_future_keyword(Prefix))
    )
  }.



%! 'PrefixedName'(?PrefixedIri:compound)// is det.
% A **prefixed name** is a *prefix label* and a *local part*,
% separated by a colon.
% The prefixed name is mapped to an IRI
% by concatenating the IRI associated by the prefix
% and the local part.
%
% ```bnf
% PrefixedName ::= PNAME_LN | PNAME_NS
% ```
%
% @compat SPARQL 1.0 [68].
% @compat SPARQL 1.1 Query [137].
% @compat Turtle 1.1 [136s].

'PrefixedName'(PrefixedIri) -->
  'PNAME_LN'(PrefixedIri).
'PrefixedName'(Prefix:'') -->
  'PNAME_NS'(Prefix).



%! simpleIRI
% # Syntax
%
% ```bnf
% simpleIRI := a finite sequence of characters matching the PN_LOCAL
%              production of [SPARQL] and not matching any of the keyword
%              terminals of the syntax
% ```
%
% ## Keywords
%
% Local parts with no prefix are expanded as if they had an initial colon
%  and must not match any keyword of this syntax.
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

simpleIRI(Iri) -->
  'PN_LOCAL'(LocalPart),
  {
    (  oms_current_keyword(LocalPart)
    ->  syntax_error(oms_current_keyword(LocalPart))
    ;   true
    ),
    rdf_global_id('':LocalPart, Iri)
  }.



%! sparqlPrefix(?PrefixLabel:atom, ?Iri:atom)// .
% ```ebnf
% sparqlPrefix ::= "PREFIX" PNAME_NS IRIREF
% ```
%
% @compat Turtle 1.1 [6s]

sparqlPrefix(PrefixLabel, Iri) -->
  "PREFIX",
  'PNAME_NS'(PrefixLabel),
  'IRIREF'(Iri),
  {rdf_register_prefix(PrefixLabel, Iri)}.





% HELPERS

oms_current_keyword('AnnotationProperty').
oms_current_keyword('Annotations').
oms_current_keyword('Asymmetric').
oms_current_keyword('Characteristics').
oms_current_keyword('Class').
oms_current_keyword('DataProperty').
oms_current_keyword('Datatype').
oms_current_keyword('DifferentFrom').
oms_current_keyword('DifferentIndividuals').
oms_current_keyword('DisjointClasses').
oms_current_keyword('DisjointProperties').
oms_current_keyword('DisjointUnionOf').
oms_current_keyword('DisjointWith').
oms_current_keyword('Domain').
oms_current_keyword('EquivalentProperties').
oms_current_keyword('EquivalentTo').
oms_current_keyword('Facts').
oms_current_keyword('Functional').
oms_current_keyword('HasKey').
oms_current_keyword('Import').
oms_current_keyword('Individual').
oms_current_keyword('InverseFunctional').
oms_current_keyword('Irreflexive').
oms_current_keyword('ObjectProperty').
oms_current_keyword('Ontology').
oms_current_keyword('Prefix').
oms_current_keyword('Range').
oms_current_keyword('Reflexive').
oms_current_keyword('SameAs').
oms_current_keyword('SameIndividual').
oms_current_keyword('SubClassOf').
oms_current_keyword('SubPropertyChain').
oms_current_keyword('SubPropertyOf').
oms_current_keyword('Symmetric').
oms_current_keyword('Transitive').
oms_current_keyword('Types').
