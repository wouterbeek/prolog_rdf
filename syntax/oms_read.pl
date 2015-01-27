:- module(
  oms_read,
  [
  ]
).

/** <module> Read Manchaster Syntax for OWL

# BNF

BNF notation in the OMS grammar is mapped onto dcg_abnf.pl
 in the following way:

| **Construct** | **Syntax**      | **`dcg_abnf`** |
|:--------------|:----------------|:---------------|
| zero or more  | curly braces    | `'*'(NT, [])`  |
| zero or one   | square brackets | `'?'(NT, [])`  |
| alternative   | vertical bar    | `(NT1 ; NT2)`  |
| grouping      | parentheses     | ???            |

# Meta-productions

```
[1]   <NT>List ::= <NT> { ',' <NT> }
[2]   <NT>2List ::= <NT> ',' <NT>List
[3]   <NT>AnnotatedList ::= [annotations] <NT> { ',' [annotations] <NT> }
```

```
[1']   '+'(NT, [separator(comma)])
[2']   'm*'(2, NT, [separator(comma)])
[3']   '+'(annotations(NT), [separator(comma)])
       annotations(NT) --> '?'(annotations, []), NT.
```

# White space

White space is allowed between any two terminals or non-terminals
 except inside nonNegativeInteger//, prefixName//, IRI//, and literal//.
White space is required between two terminals or non-terminals if
 its removal could cause ambiguity.
Generally this means requiring white space except before and after
 punctuation (e.g., commas, parentheses, braces, and brackets). 

--

@author Wouter Beek
@compat [OWL 2 Web Ontology Language Manchester Syntax (Second Edition)](http://www.w3.org/TR/2012/NOTE-owl2-manchester-syntax-20121211/)
@version 2014/12
*/

:- use_module(plDcg(dcg_bracket)).

:- use_module(plRdf(syntax/sw_bnode)).
:- use_module(plRdf(syntax/sw_char)).
:- use_module(plRdf(syntax/sw_iri)).





%! entity// .
% ```bnf
% entity ::=   'Datatype' '(' Datatype ')'
%            | 'Class' '(' classIRI ')'
%            | 'ObjectProperty' '(' objectPropertyIRI ')'
%            | 'DataProperty' '('dataPropertyIRI ')'
%            | 'AnnotationProperty' '(' annotationPropertyIRI ')'
%            | 'NamedIndividual' '(' individualIRI ')'
% ```
% 
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

entity(Iri) --> "Datatype", bracketed('Datatype'(Iri)).
entity(Iri) --> "Class", bracketed(classIRI(Iri)).
entity(Iri) --> "ObjectProperty", bracketed(objectPropertyIRI(Iri)).
entity(Iri) --> "AnnotationProperty", bracketed(annotationPropertyIRI(Iri)).
entity(Iri) --> "NamedIndividual", bracketed(individualIRI(Iri)).



%! individual(?Individual:or([bnode,iri]))// .
% ```bnf
% individual ::= individualIRI | nodeID
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

individual(Iri) --> individualIRI(Iri).
individual(BNode) --> nodeID(manchester, BNode).

