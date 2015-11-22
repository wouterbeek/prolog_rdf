:- module(
  sparql11_token,
  [
    'BLANK_NODE_LABEL'//1, % ?BlankNodeLabel:atom
    'IRIREF'//1 % ?Iri:atom
  ]
).
:- reexport(library(dcg/sparql10_token), [
     'ANON'//1, % ?BlankNode:bnode
     'BlankNode'//1, % ?BlankNode:bnode
     'BooleanLiteral'//1, % ?Boolean:boolean
     'DECIMAL'//1, % ?Decimal:rational
     'DECIMAL_NEGATIVE'//1, % ?Decimal:rational
     'DECIMAL_POSITIVE'//1, % ?Decimal:rational
     'DOUBLE'//1, % ?Double:float
     'DOUBLE_NEGATIVE'//1, % ?Double:float
     'DOUBLE_POSITIVE'//1, % ?Double:float
     'EXPONENT'//1, % ?Exponent:integer
     'INTEGER'//1, % ?Integer:integer
     'INTEGER_NEGATIVE'//1, % ?Integer:negative_integer
     'INTEGER_POSITIVE'//1, % ?Integer:positive_integer
     iri//1, % ?Iri:atom
     'IRI_REF'//1, % ?Iri:atom
     'IRIref'//1, % ?Iri:atom
     'LANGTAG'//1, % ?LanguageTag:list(atom)
     'NumericLiteral'//1, % ?Literal:compound
     'NumericLiteralNegative'//1, % ?Literal:compound
     'NumericLiteralPositive'//1, % ?Literal:compound
     'NumericLiteralUnsigned'//1, % ?Literal:compound
     'PNAME_LN'//1, % ?Iri:atom
     'PNAME_NS'//1, % ?Prefix:atom
     'PrefixName'//1, % ?Iri:atom
     'RDFLiteral'//1, % ?Literal:compound
     'String'//1, % ?String:atom
     'STRING_LITERAL_LONG1'//1, % ?String:atom
     'STRING_LITERAL_LONG2'//1, % ?String:atom
     'STRING_LITERAL1'//1, % ?String:atom
     'STRING_LITERAL2'//1, % ?String:atom
   ]).

/** <module> SPARQL 1.1: Tokens

@author Wouter Beek
@compat SPARQL 1.1
@see http://www.w3.org/TR/sparql11-query/#grammar
@version 2015/11
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/sparql11_code)).





%! 'BLANK_NODE_LABEL'(?BlankNodeLabel:atom)// .
% Blank node labels are written as `_:abc` for a blank node with label `abc`.
%
% The same blank node label cannot be used
% in two different basic graph patterns in the same query.
%
% ```ebnf
% BLANK_NODE_LABEL ::= '_:'
%                      ( PN_CHARS_U | [0-9] )
%                      ( ( PN_CHARS | '.' )* PN_CHARS )?
% ```

'BLANK_NODE_LABEL'(A) --> "_:", !, dcg_atom(blank_node_label_codes1, A).
blank_node_label_codes1([H|T])   --> 'PN_CHARS_U'(H), !, blank_node_label_codes2(T).
blank_node_label_codes1([H|T])   --> digit(H),        !, blank_node_label_codes2(T).
blank_node_label_codes2([0'.|T]) --> ".",             !, blank_node_label_codes3(T).
blank_node_label_codes2([H|T])   --> 'PN_CHARS_U'(H), !, blank_node_label_codes4(T).
blank_node_label_codes2([])      --> "".
blank_node_label_codes3([0'.|T]) --> ".",             !, blank_node_label_codes4(T).
blank_node_label_codes3([H|T])   --> 'PN_CHARS_U'(H), !, blank_node_label_codes4(T).
blank_node_label_codes4([0'.|T]) --> ".",             !, blank_node_label_codes4(T).
blank_node_label_codes4([H|T])   --> 'PN_CHARS'(H),   !, blank_node_label_codes4(T).
blank_node_label_codes4([])      --> "".



%! 'IRIREF'(?Iri:atom)// .
% ```ebnf
% IRIREF ::= '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% ```
%
% @tbd What about DELETE (decimal 127)?

'IRIREF'(Iri) --> "<", dcg_atom(iriref_codes, Iri), ">".
iriref_codes([H|T]) --> iriref_code(H), !, iriref_codes(T).
iriref_codes([])    --> "".
iriref_code(_) --> 'CTL', !, {fail}.
iriref_code(_) --> "<",   !, {fail}.
iriref_code(_) --> ">",   !, {fail}.
iriref_code(_) --> "\"",  !, {fail}.
iriref_code(_) --> "{",   !, {fail}.
iriref_code(_) --> "}",   !, {fail}.
iriref_code(_) --> "|",   !, {fail}.
iriref_code(_) --> "^",   !, {fail}.
iriref_code(_) --> "\`",  !, {fail}.
iriref_code(_) --> "\\",  !, {fail}.
iriref_code(C) --> 'UCHAR'(C).
iriref_code(C) --> [C].
