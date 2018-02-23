:- module(
  turtle11,
  [
    'DECIMAL'//1,                          % -Decimal
    'INTEGER'//1,                          % -I
    'IRIREF'//1,                           % -Iri
    literal//1,                            % -Lit
    'NumericLiteral'//1,                   % -Lit
    prefixID//2,                           % -Alias, -Prefix
    sparqlPrefix//2,                       % -Alias, -Prefix
    'String'//1,                           % -String
    'STRING_LITERAL_QUOTE'//1,             % -String
    'STRING_LITERAL_SINGLE_QUOTE'//1,      % -String
    'STRING_LITERAL_LONG_SINGLE_QUOTE'//1, % -String
    'STRING_LITERAL_LONG_QUOTE'//1         % -String
  ]
).
:- reexport(library(dcg/sparql10), [
   ]).
:- reexport(library(dcg/sparql11), [
     'BooleanLiteral'//1, % -Lit
     'DOUBLE'//1,         % ?Number
     'ECHAR'//1,          % ?C
     'LANGTAG'//1,        % -LTag:list(atom)
     'PNAME_LN'//1,       % -Prefix
     'PNAME_NS'//1,       % -Alias
     iri//1               % -Iri
   ]).
:- reexport(library(dcg/turtle10), [
     nodeID//1 % -BNode
   ]).

/** <module> Turtle 1.1

@author Wouter Beek
@compat Turtle 1.1
@version 2015/11-2016/01, 2016/05
*/

:- use_module(library(clpfd)).
:- use_module(library(dcg)).






%! 'DECIMAL'(-Decimal:rational)// is det.
% ```ebnf
% DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
% ```

'DECIMAL'(N) -->
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1} ; {Sg = 1}),
  dcg_integer(*(digit), I),
  ".",
  +(digit, Weights2),
  {
    fractional_weights(Frac, Weights2),
    N is Sg * (I + Frac)
  }.



%! 'INTEGER'(-Integer:integer)// is det.
% ```ebnf
% INTEGER ::= [+-]? [0-9]+
% ```

'INTEGER'(I2) -->
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1} ; {Sg = 1}),
  dcg_integer(+(digit), I1),
  {I2 #= Sg * I1}.



%! 'IRIREF'(-Iri:atom)// is det.
% ```ebnf
% IRIREF ::= '<' ( [^#x00-#x20<>"{}|^`\] | UCHAR )* '>'
% ```
%
% What about DELETE (decimal 127)?

'IRIREF'(Iri) --> "<", dcg_atom(iriref_codes, Iri), ">".
iriref_codes([H|T]) --> iriref_code(H), !, iriref_codes(T).
iriref_codes([])    --> "".
iriref_code(_) --> between(0x00, 0x20), !, {fail}.
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
  {rdf_create_alias(PrefixLabel, Iri)}.



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
  {rdf_create_alias(PrefixLabel, Iri)}.



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

'STRING_LITERAL_QUOTE'(S) --> 'STRING_LITERAL'("\"", S).



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

'STRING_LITERAL_SINGLE_QUOTE'(S) --> 'STRING_LITERAL'("'", S).



%! 'STRING_LITERAL_LONG_SINGLE_QUOTE'(-String:string)// is det.
% ```ebnf
% STRING_LITERAL_LONG_SINGLE_QUOTE ::= "'''"
%                                      ( ("'" | "''")? ([^'\] | ECHAR | UCHAR) )*
%                                      "'''"
% ```
%
% This different from SPARQL 1.0 [89] and SPARQL 1.1 [158]
% in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_LONG_SINGLE_QUOTE'(S) --> 'STRING_LITERAL_LONG'("'", S).



%! 'STRING_LITERAL_LONG_QUOTE'(-String:string)// is det.
% ```ebnf
% STRING_LITERAL_LONG_QUOTE ::= '"""'
%                               ( ('"' | '""')? ([^"\] | ECHAR | UCHAR) )*
%                               '"""'
% ```
%
% This different from SPARQL 1.0 [90] and SPARQL 1.1 [159]
% in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_LONG_QUOTE'(S) --> 'STRING_LITERAL_LONG'("\"", S).



%! 'UCHAR'(?Code:code)// .
% ```ebnf
% UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ```

'UCHAR'(Code) -->
  "\\u",
  dcg_integer(#(4, xdigit), 16, Code).
'UCHAR'(Code) -->
  "\\U",
  dcg_integer(#(8, xdigit), 16, Code).





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
