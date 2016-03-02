:- module(
  turtle11,
  [
    'DECIMAL'//1, % -Decimal:rational
    'INTEGER'//1, % -Integer:integer
    'IRIREF'//1, % -Iri:atom
    literal//1, % -Literal:compound
    'NumericLiteral'//1, % -Literal:compound
    prefixID//2, % -PrefixLabel:atom
                 % -Iri:atom
    sparqlPrefix//2, % -PrefixLabel:atom
                     % -Iri:atom
    'String'//1, % -String:atom
    'STRING_LITERAL_QUOTE'//1, % -String:atom 
    'STRING_LITERAL_SINGLE_QUOTE'//1, % -String:atom
    'STRING_LITERAL_LONG_SINGLE_QUOTE'//1, % -String:atom
    'STRING_LITERAL_LONG_QUOTE'//1 % -String:atom
  ]
).
:- reexport(library(dcg/sparql10), [
   ]).
:- use_module(library(dcg/sparql11), [
     'BooleanLiteral'//1, % -Literal:compound
     'DOUBLE'//1, % ?Number:rational
     'ECHAR'//1, % ?Code:code
     'HEX'//1, % ?Weight:between(0,15)
     'LANGTAG'//1, % -LanguageTag:list(atom)
     'PNAME_LN'//1, % -Iri:atom
     'PNAME_NS'//1, % -Prefix:atom
     iri//1 % -Iri:atom
   ]).
:- reexport(library(dcg/turtle10), [
     nodeID//1 % -BlankNode:bnode
   ]).

/** <module> Turtle 1.1

@author Wouter Beek
@compat Turtle 1.1
@version 2015/11-2016/01
*/

:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(rdf11/rdf11)).






%! 'DECIMAL'(-Decimal:rational)// is det.
% ```ebnf
% DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
% ```

'DECIMAL'(N) -->
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1} ; {Sg = 1}),
  *(digit, Ds1),
  {pos_sum(Ds1, I)},
  ".",
  +(digit, Ds2),
  {
    pos_frac(Ds2, Frac),
    N is Sg * (I + Frac)
  }.



%! 'INTEGER'(-Integer:integer)// is det.
% ```ebnf
% INTEGER ::= [+-]? [0-9]+
% ```

'INTEGER'(I) -->
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1} ; {Sg = 1}),
  +(digit, Ds),
  {pos_sum(Ds, I0)},
  {I is Sg * I0}.



%! 'IRIREF'(-Iri:atom)// is det.
% ```ebnf
% IRIREF ::= '<' ( [^#x00-#x20<>"{}|^`\] | UCHAR )* '>'
% ```
%
% What about DELETE (decimal 127)?

'IRIREF'(Iri) --> "<", dcg_atom(iriref_codes, Iri), ">".
iriref_codes([H|T]) --> iriref_code(H), !, iriref_codes(T).
iriref_codes([])    --> "".
iriref_code(_) --> [C], {between(0x00, 0x20, C)}, !, {fail}.
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



%! literal(-Literal:compound)// is det.
% ```ebnf
% literal ::= RDFLiteral | NumericLiteral | BooleanLiteral
% ```

literal(Lit) --> 'RDFLiteral'(Lit).
literal(Lit) --> 'NumericLiteral'(Lit).
literal(Lit) --> 'BooleanLiteral'(Lit).



%! 'NumericLiteral'(-Literal:compound)// is det.
% ```ebnf
% NumericLiteral ::= INTEGER | DECIMAL | DOUBLE
% ```
%
% As a convenience, integers / decimal numbers can be written directly
% (without quotation marks and an explicit datatype IRI)
% and are interpreted as typed literals of datatype
% `xsd:integer`/`xsd:decimal`/`xsd:double`.
%
% The following types of numbers are distinguished:
%   - *Integers* have not exponent and no decimal point.
%   - *Decimals* have a decimal point and no exponent.
%   - *Doubles* have an exponent.

'NumericLiteral'(N^^xsd:decimal) --> 'DECIMAL'(N).
'NumericLiteral'(N^^xsd:double)  --> 'DOUBLE'(N).
'NumericLiteral'(N^^xsd:integer) --> 'INTEGER'(N).



%! prefixID(-PrefixLabel:atom, -Iri:atom)// is det.
% ```ebnf
% prefixID ::= '@prefix' PNAME_NS IRIREF '.'
% ```

prefixID(PrefixLabel, Iri) -->
  "@prefix",
  'PNAME_NS'(PrefixLabel),
  'IRIREF'(Iri),
  ".",
  {rdf_register_prefix(PrefixLabel, Iri)}.



%! 'RDFLiteral'(-Literal:compound)// is det.
% ```ebnf
% RDFLiteral ::= String ( LANGTAG | ( '^^' iri ) )?
% ```

'RDFLiteral'(Lit) -->
  'String'(Lex),
  (   "^^"
  ->  iri(D),
      {Lit = Lex^^D}
  ;   'LANGTAG'(LTag),
      {atomic_list_concat(LTag, -, LTag0), Lit = Lex@LTag0}
  ;   {Lit = Lex^^xsd:string}
  ).



%! sparqlPrefix(-PrefixLabel:atom, -Iri:atom)// is det.
% ```ebnf
% sparqlPrefix ::= "PREFIX" PNAME_NS IRIREF
% ```

sparqlPrefix(PrefixLabel, Iri) -->
  "PREFIX",
  'PNAME_NS'(PrefixLabel),
  'IRIREF'(Iri),
  {rdf_register_prefix(PrefixLabel, Iri)}.



%! 'String'(-String:string)// is det.
% ```ebnf
% String ::= STRING_LITERAL_QUOTE
%          | STRING_LITERAL_SINGLE_QUOTE
%          | STRING_LITERAL_LONG_SINGLE_QUOTE
%          | STRING_LITERAL_LONG_QUOTE
% ```

'String'(S) --> 'STRING_LITERAL_QUOTE'(S).
'String'(S) --> 'STRING_LITERAL_SINGLE_QUOTE'(S).
'String'(S) --> 'STRING_LITERAL_LONG_SINGLE_QUOTE'(S).
'String'(S) --> 'STRING_LITERAL_LONG_QUOTE'(S).



%! 'STRING_LITERAL_QUOTE'(-String:string)// is det.
% ```ebnf
% STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
%                          /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
% ```
%
% This different from SPARQL 1.0 [88] and SPARQL 1.1 [157]
% in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_QUOTE'(S) --> 'STRING_LITERAL'(double_quote, S).



%! 'STRING_LITERAL_SINGLE_QUOTE'(-String:string)// is det.
% ```ebnf
% STRING_LITERAL_SINGLE_QUOTE ::= "'"
%                                 ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)*
%                                 "'"
%                                 /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
% ```
%
% This different from SPARQL 1.0 [87] and SPARQL 1.1 [156]
% in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_SINGLE_QUOTE'(S) --> 'STRING_LITERAL'(single_quote, S).



%! 'STRING_LITERAL_LONG_SINGLE_QUOTE'(-String:string)// is det.
% ```ebnf
% STRING_LITERAL_LONG_SINGLE_QUOTE ::= "'''"
%                                      ( ("'" | "''")? ([^'\] | ECHAR | UCHAR) )*
%                                      "'''"
% ```
%
% This different from SPARQL 1.0 [89] and SPARQL 1.1 [158]
% in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_LONG_SINGLE_QUOTE'(S) --> 'STRING_LITERAL_LONG'(single_quote, S).



%! 'STRING_LITERAL_LONG_QUOTE'(-String:string)// is det.
% ```ebnf
% STRING_LITERAL_LONG_QUOTE ::= '"""'
%                               ( ('"' | '""')? ([^"\] | ECHAR | UCHAR) )*
%                               '"""'
% ```
%
% This different from SPARQL 1.0 [90] and SPARQL 1.1 [159]
% in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_LONG_QUOTE'(S) --> 'STRING_LITERAL_LONG'(double_quote, S).



%! 'UCHAR'(?Code:code)// .
% ```ebnf
% UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ```

'UCHAR'(C) --> "\\u", #(4, 'HEX', Ds), {pos_sum(Ds, 16, C)}.
'UCHAR'(C) --> "\\U", #(8, 'HEX', Ds), {pos_sum(Ds, 16, C)}.





% HELPERS %

'STRING_LITERAL'(Q, S) -->
  quoted(Q, string_literal_codes(Q, Cs)),
  {string_codes(S, Cs)}.
string_literal_codes(Q, _)     --> Q,          !, {fail}.
string_literal_codes(_, _)     --> [0x5C],     !, {fail}.
string_literal_codes(_, _)     --> [0xA],      !, {fail}.
string_literal_codes(_, _)     --> [0xD],      !, {fail}.
string_literal_codes(Q, [H|T]) --> 'ECHAR'(H), !, string_literal_codes(Q, T).
string_literal_codes(Q, [H|T]) --> 'UCHAR'(H), !, string_literal_codes(Q, T).
string_literal_codes(_, [])    --> "".


'STRING_LITERAL_LONG'(Q, S) -->
  quoted(3, Q, string_literal_long_codes(Q, Cs)),
  {string_codes(S, Cs)}.
string_literal_long_codes(_, _)     --> "\\",       !, {fail}.
string_literal_long_codes(Q, _)     --> Q, Q, Q,    !, {fail}.
string_literal_long_codes(Q, [H|T]) --> 'ECHAR'(H), !, string_literal_long_codes(Q, T).
string_literal_long_codes(Q, [H|T]) --> 'UCHAR'(H), !, string_literal_long_codes(Q, T).
string_literal_long_codes(_, [])    --> "".
