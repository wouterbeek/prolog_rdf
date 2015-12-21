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
     'IRI_REF'//1, % ?Iri:atom
     'IRIref'//1, % ?Iri:atom
     'LANGTAG'//1, % ?LanguageTag:list(atom)
     'NumericLiteral'//1, % ?Literal:compound
     'NumericLiteralNegative'//1, % ?Literal:compound
     'NumericLiteralPositive'//1, % ?Literal:compound
     'NumericLiteralUnsigned'//1, % ?Literal:compound
     'PNAME_LN'//1, % ?Iri:atom
     'PNAME_NS'//1, % ?Prefix:atom
     'RDFLiteral'//1, % ?Literal:compound
     'String'//1, % ?String:atom
     'STRING_LITERAL_LONG1'//1, % ?String:atom
     'STRING_LITERAL_LONG2'//1, % ?String:atom
     'STRING_LITERAL1'//1, % ?String:atom
     'STRING_LITERAL2'//1 % ?String:atom
   ]).

/** <module> SPARQL 1.1: Tokens

@author Wouter Beek
@compat SPARQL 1.1
@see http://www.w3.org/TR/sparql11-query/#grammar
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
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

'BLANK_NODE_LABEL'(A) -->
  "_:",
  ('PN_CHARS_U'(H), ! ; digit(_, H)),
  (*(pn_chars_dot, T), 'PN_CHARS'(X) -> {append([H|T], [X], Cs)} ; {Cs = [H]}),
  {atom_codes(A, Cs)}.



%! 'IRIREF'(?Iri:atom)// .
% ```ebnf
% IRIREF ::= '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% ```
%
% @tbd What about DELETE (decimal 127)?

'IRIREF'(A) --> "<", *(iriref_code, Cs), ">", {atom_codes(A, Cs)}.
iriref_code(C) --> [C], {between(0x00, 0x20, C)}, !, {fail}.
iriref_code(_) --> "<",                           !, {fail}.
iriref_code(_) --> ">",                           !, {fail}.
iriref_code(_) --> "\"",                          !, {fail}.
iriref_code(_) --> "{",                           !, {fail}.
iriref_code(_) --> "}",                           !, {fail}.
iriref_code(_) --> "|",                           !, {fail}.
iriref_code(_) --> "^",                           !, {fail}.
iriref_code(_) --> "\`",                          !, {fail}.
iriref_code(_) --> "\\",                          !, {fail}.
iriref_code(C) --> 'UCHAR'(C).
iriref_code(C) --> [C].
