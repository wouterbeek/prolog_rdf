:- module(
  sparql10,
  [
    'ANON'//1, % ?BlankNode:bnode
    'BLANK_NODE_LABEL'//1, % -BlankNode:atom
    'BlankNode'//1, % ?BlankNode:bnode
    'BooleanLiteral'//1, % ?Literal:compound
    'DECIMAL'//1, % ?Decimal:rational
    'DECIMAL_NEGATIVE'//1, % ?Decimal:rational
    'DECIMAL_POSITIVE'//1, % ?Decimal:rational
    'DOUBLE'//1, % ?Double:float
    'DOUBLE_NEGATIVE'//1, % ?Double:float
    'DOUBLE_POSITIVE'//1, % ?Double:float
    'ECHAR'//1, % ?Code:code
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
    'PN_CHARS'//1, % ?Code:code
    'PN_CHARS_BASE'//1, % ?Code:code
    'PN_CHARS_U'//1, % ?Code:code
    'PN_LOCAL'//1, % ?LocalPart:atom
    'PN_PREFIX'//1, % ?Prefix:atom
    'PNAME_LN'//1, % ?Iri:atom
    'PNAME_NS'//1, % ?Prefix:atom
    'PrefixedName'//1, % ?Iri:atom
    'RDFLiteral'//1, % ?Literal:compound
    'String'//1, % ?String:atom
    'STRING_LITERAL_LONG1'//1, % ?String:atom
    'STRING_LITERAL_LONG2'//1, % ?String:atom
    'STRING_LITERAL1'//1, % ?String:atom
    'STRING_LITERAL2'//1, % ?String:atom
    'WS'//0
  ]
).

/** <module> SPARQL 1.0

@author Wouter Beek
@compat SPARQL 1.0
@deprecated
@version 2015/11-2016/01
*/

:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_quote)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(math/rational_ext)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read), []).

:- meta_predicate
    'STRING_LITERAL'(//, -, ?, ?),
    'string_literal_codes'(//, -, ?, ?),
    'STRING_LITERAL_LONG'(//, -, ?, ?),
    'string_literal_long_codes'(//, -, ?, ?).

:- rdf_meta
   'NumericLiteral'(o, ?, ?),
   'NumericLiteralNegative'(o, ?, ?),
   'NumericLiteralPositive'(o, ?, ?),
   'NumericLiteralUnsigned'(o, ?, ?).





%! 'ANON'(-BlankNode:atom)// is det.
% A blank node that is used in only one place in the query syntax
% can be indicated with the notation `[]`.
%
% A unique blank node will be used to form the triple pattern.
%
% ```ebnf
% [94]   ANON ::= '[' WS* ']'
% ```

'ANON'(A) --> "[", *('WS'), "]", {rdf_create_bnode(A)}.



%! 'BLANK_NODE_LABEL'(-BlankNodeLabel:atom)// is det.
% ```ebnf
% [73]   BLANK_NODE_LABEL ::= '_:' PN_LOCAL
% ```

'BLANK_NODE_LABEL'(A) --> "_:", 'PN_LOCAL'(A).



%! 'BlankNode'(-BlankNode:atom)// is det.
% Blank nodes are indicated by either the label form,
% such as `_:abc`, or the abbreviated form `[]`.
%
% ```ebnf
% [69]   BlankNode ::= BLANK_NODE_LABEL | ANON
% ```

'BlankNode'(A) --> 'BLANK_NODE_LABEL'(A), !.
'BlankNode'(A) --> 'ANON'(A).



%! 'BooleanLiteral'(-Literal:compound)// is det.
% As a convenience, values of type `xsd:boolean`
% can also be written as `true` or `false`
% (without quotation marks and an explicit datatype IRI).
%
% ```ebnf
% [65]   BooleanLiteral ::= 'true' | 'false'
% ```

'BooleanLiteral'(false^^xsd:boolean) --> "false", !.
'BooleanLiteral'(true^^xsd:boolean)  --> "true".



%! 'DECIMAL'(-Decimal:rational)// is det.
% ```ebnf
% [78]   DECIMAL ::= [0-9]* '.' [0-9]+
% ```

'DECIMAL'(Rat) -->
  *(digit, Ds1),
  {pos_sum(Ds1, I)},
  ".",
  +(digit, Ds2),
  {pos_frac(Ds2, Frac)},
  {rational_parts(Rat, I, Frac)}.



%! 'DECIMAL_NEGATIVE'(-Decimal:rational)// is det.
% ```bnf
% [84]   DECIMAL_NEGATIVE ::= '-' DECIMAL
% ```

'DECIMAL_NEGATIVE'(Rat) --> "-", 'DECIMAL'(Rat0), {Rat is -Rat0}.



%! 'DECIMAL_POSITIVE'(-Decimal:rational)// is det.
% ```bnf
% [81]   DECIMAL_POSITIVE ::= '+' INTEGER
% ```

'DECIMAL_POSITIVE'(Rat) --> "+", 'INTEGER'(Rat).



%! 'DOUBLE'(-Double:float)// is det.
% ```ebnf
% [79]   DOUBLE ::= [0-9]+ '.' [0-9]* EXPONENT
%                 | '.' [0-9]+ EXPONENT
%                 | [0-9]+ EXPONENT
% ```
%
% @compat Turtle 1.1 [21]

'DOUBLE'(F) -->
  (   "."
  ->  +(digit, Ds2),
      {pos_frac(Ds2, Frac)}
  ;   +(digit, Ds1),
      {pos_sum(Ds1, I)},
      ("." -> *(digit, Ds2) ; {Ds2 = []}),
      {pos_frac(Ds2, Frac)}
  ),
  'EXPONENT'(Exp),
  {F is float(I + Frac) * 10 ^ Exp}.



%! 'DOUBLE_NEGATIVE'(-Double:float)// is det.
% ```bnf
% [85]   DOUBLE_NEGATIVE ::= '-' DOUBLE
% ```

'DOUBLE_NEGATIVE'(F) --> "-", 'DOUBLE'(F0), {F is -F0}.



%! 'DOUBLE_POSITIVE'(-Double:float)// is det.
% ```bnf
% [82]   DOUBLE_POSITIVE ::= '+' DOUBLE
% ```

'DOUBLE_POSITIVE'(F) --> "+", 'DOUBLE'(F).



%! 'ECHAR'(?Code:code)// .
% ```bnf
% ECHAR ::= '\' [tbnrf"'\]
% ```

'ECHAR'(C) --> "\\", echar_code(C).
echar_code(0'\t) --> "\t".
echar_code(0'\b) --> "\b".
echar_code(0'\n) --> "\n".
echar_code(0'\r) --> "\r".
echar_code(0'\f) --> "\f".
echar_code(0'")  --> "\"".   %"
echar_code(0'')  --> "'".
echar_code(0'\\) --> "\\".



%! 'EXPONENT'(-Exponent:integer)// is det.
% ```ebnf
% [86]   EXPONENT ::= [eE] [+-]? [0-9]+
% ```
%
% @compat SPARQL 1.0 [86]
% @compat SPARQL 1.1 Query [155]
% @compat Turtle 1.1 [154s]

'EXPONENT'(Exp) -->
  ("e" ; "E"), !,
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1} ; {Sg = 1}),
  +(digit, Ds),
  {pos_sum(Ds, Exp0)},
  {Exp is Sg * Exp0}.



%! 'INTEGER'(-Integer:integer)// is det.
% ```ebnf
% [77]   INTEGER ::= [0-9]+
% ```

'INTEGER'(I) --> +(digit, Ds), {pos_sum(Ds, I)}.



%! 'INTEGER_NEGATIVE'(-Integer:negative_integer)// is det.
% ```bnf
% [83]   INTEGER_NEGATIVE ::= '-' INTEGER
% ```

'INTEGER_NEGATIVE'(I) --> "-", 'INTEGER'(I0), {I is -I0}.



%! 'INTEGER_POSITIVE'(-Integer:positive_integer)// is det.
% ```bnf
% [80]   INTEGER_POSITIVE ::= '+' INTEGER
% ```

'INTEGER_POSITIVE'(I) --> "+", 'INTEGER'(I).



%! 'IRI_REF'(-Iri:atom)// is det.
% ```ebnf
% [70]   IRI_REF ::=  '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% ```
%
% @tbd What about DELETE (decimal 127)?

'IRI_REF'(A) --> "<", *(iriref_code, Cs), ">", {atom_codes(A, Cs)}.
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
iriref_code(C) --> [C].



%! 'IRIref'(-Iri:atom)// is det.
% ```ebnf
% [67]   IRIref ::= IRI_REF | PrefixedName
% ```
%
% @deprecated Use iri//1 instead.

'IRIref'(Iri) --> 'IRI_REF'(Iri), !.
'IRIref'(Iri) --> 'PrefixedName'(Iri).



%! 'LANGTAG'(-LanguageTag:list(atom))// is det.
% ```ebnf
% [76]   LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
% ```

'LANGTAG'([H|T]) -->
  "@",
  +(alpha, Cs), {atom_codes(H, Cs)}, !,
  subtags(T).
subtags([H|T]) -->
  "-", !,
  +(alphadigit, Cs), {atom_codes(H, Cs)},
  subtags(T).
subtags([]) --> "".



%! 'NumericLiteral'(-Literal:compound)// is det.
% ```ebnf
% [61]   NumericLiteral ::= INTEGER | DECIMAL | DOUBLE
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

'NumericLiteral'(Lit) --> 'NumericLiteralNegative'(Lit), !.
'NumericLiteral'(Lit) --> 'NumericLiteralPositive'(Lit), !.
'NumericLiteral'(Lit) --> 'NumericLiteralUnsigned'(Lit).



%! 'NumericLiteralNegative'(-Literal:compound)// is det.
% ```ebnf
% [64]   NumericLiteralNegative ::= INTEGER_NEGATIVE
%                                 | DECIMAL_NEGATIVE
%                                 | DOUBLE_NEGATIVE
% ```

'NumericLiteralNegative'(N^^xsd:integer) --> 'INTEGER_NEGATIVE'(N), !.
'NumericLiteralNegative'(N^^xsd:decimal) --> 'DECIMAL_NEGATIVE'(N), !.
'NumericLiteralNegative'(N^^xsd:double)  --> 'DOUBLE_NEGATIVE'(N).



%! 'NumericLiteralPositive'(-Literal:compound)// is det.
% ```ebnf
% [63]   NumericLiteralPositive ::= INTEGER_POSITIVE
%                                 | DECIMAL_POSITIVE
%                                 | DOUBLE_POSITIVE
% ```

'NumericLiteralPositive'(N^^xsd:integer) --> 'INTEGER_POSITIVE'(N), !.
'NumericLiteralPositive'(N^^xsd:decimal) --> 'DECIMAL_POSITIVE'(N), !.
'NumericLiteralPositive'(N^^xsd:double)  --> 'DOUBLE_POSITIVE'(N).



%! 'NumericLiteralUnsigned'(-Literal:compound)// is det.
% ```ebnf
% [62]   NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
% ```

'NumericLiteralUnsigned'(N^^xsd:integer) --> 'INTEGER'(N), !.
'NumericLiteralUnsigned'(N^^xsd:decimal) --> 'DECIMAL'(N), !.
'NumericLiteralUnsigned'(N^^xsd:double)  --> 'DOUBLE'(N).



%! 'PN_CHARS'(?Code:code)// .
% ```
% PN_CHARS ::= PN_CHARS_U
%            | '-'
%            | [0-9]
%            | #x00B7
%            | [#x300-#x36F]
%            | [#x203F-#x2040]
% ```

'PN_CHARS'(C)      --> 'PN_CHARS_U'(C).
'PN_CHARS'(0'-)    --> "-".
'PN_CHARS'(C)      --> digit(C).
'PN_CHARS'(0x00BF) --> [0x00B7].
'PN_CHARS'(C)      --> [C], {between(0x0300, 0x036F, C)}.
'PN_CHARS'(C)      --> [C], {between(0x203F, 0x2040, C)}.



%! 'PN_CHARS_BASE'(?Code:code)// .
% ```
% PN_CHARS_BASE ::= [A-Z]
%                 | [a-z]
%                 | [#x00C0-#x00D6]
%                 | [#x00D8-#x00F6]
%                 | [#x00F8-#x02FF]
%                 | [#x0370-#x037D]
%                 | [#x037F-#x1FFF]
%                 | [#x200C-#x200D]
%                 | [#x2070-#x218F]
%                 | [#x2C00-#x2FEF]
%                 | [#x3001-#xD7FF]
%                 | [#xF900-#xFDCF]
%                 | [#xFDF0-#xFFFD]
%                 | [#x10000-#xEFFFF]
% ```
%
% @see Almost the same as XML 1.0.5 and 1.1.2,
%      but without colon and underscore.

'PN_CHARS_BASE'(C) --> alpha(C).
'PN_CHARS_BASE'(C) --> [C], {between(0x00C0,  0x00D6,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0x00D8,  0x00F6,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0x00F8,  0x02FF,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0x0370,  0x037D,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0x037F,  0x1FFF,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0x200C,  0x200D,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0x2070,  0x218F,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0x2C00,  0x2FEF,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0x3001,  0xD7FF,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0xF900,  0xFDCF,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0xFDF0,  0xFFFD,  C)}.
'PN_CHARS_BASE'(C) --> [C], {between(0x10000, 0xEFFFF, C)}.



%! 'PN_CHARS_U'(?Code:code)// .
% ```
% PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ```
%
% Different from N-Quads and N-Triples where the colon is included as well.

'PN_CHARS_U'(C)   --> 'PN_CHARS_BASE'(C).
'PN_CHARS_U'(0'_) --> "_".



%! 'PN_LOCAL'(-LocalPart:atom)// is det.
% The **local part** of a prefixed name.
%
% ```ebnf
% [100]   PN_LOCAL ::= ( PN_CHARS_U | [0-9] )
%                      (( PN_CHARS | '.')* PN_CHARS)?
% ```

'PN_LOCAL'(A) -->
  ('PN_CHARS_U'(H), ! ; digit(_, H)),
  (   *(pn_chars_dot, T), 'PN_CHARS'(X)
  ->  {append([H|T], [X], Cs)}
  ;   {Cs = [H]}
  ),
  {atom_codes(A, Cs)}.



%! 'PN_PREFIX'(-Prefix:atom)// is det.
% ```ebnf
% [99]   PN_PREFIX ::= PN_CHARS_BASE ( ( PN_CHARS | '.' )* PN_CHARS )?
% ```

'PN_PREFIX'(A) -->
  'PN_CHARS_BASE'(H),
  (*(pn_chars_dot, T), 'PN_CHARS'(X) -> {append([H|T], [X], Cs)} ; {Cs = [H]}),
  {atom_codes(A, Cs)}.



%! 'PNAME_LN'(-Iri:atom)// is det.
% ```ebnf
% [72]   PNAME_LN ::= PNAME_NS PN_LOCAL
% ```

'PNAME_LN'(Iri) -->
  'PNAME_NS'(Prefix),
  'PN_LOCAL'(Local),
  {rdf_global_id(Prefix:Local, Iri)}.



%! 'PNAME_NS'(?Prefix:atom)// is det.
% An IRI prefix label.
%
% Notice that the empty string is also a prefix label.
%
% ```ebnf
% [71]   PNAME_NS ::= PN_PREFIX? ':'
% ```

'PNAME_NS'(Prefix) --> 'PN_PREFIX'(Prefix), !, ":".
'PNAME_NS'('')     --> ":".



%! 'PrefixedName'(-Iri:atom)// is det.
% A **prefixed name** is a *prefix label* and a *local part*,
% separated by a colon.
% The prefixed name is mapped to an IRI
% by concatenating the IRI associated by the prefix
% and the local part.
%
% ```ebnf
% [68]   PrefixedName ::= PNAME_LN | PNAME_NS
% ```

'PrefixedName'(Iri) --> 'PNAME_LN'(Iri), !.
'PrefixedName'(Iri) --> 'PNAME_NS'(Prefix), {rdf_global_id(Prefix:'', Iri)}.



%! 'RDFLiteral'(-Literal:compound)// is det.
% The general syntax for RDF literals.
% The consist of a string (enclosed in either double or single quotes),
% with either an optional language tag (introduced by `@`)
% or an optional datatype IRI or prefixed name (introduced by `^^`).
%
% ```ebnf
% [60]   RDFLiteral ::= String ( LANGTAG | ( '^^' IRIref ) )?
% ```

'RDFLiteral'(Lit) -->
  'String'(Lex),
  (   "^^"
  ->  'IRIref'(D), {Lit = Lex^^D}
  ;   'LANGTAG'(LTag),
      {atomic_list_concat(LTag, -, LTag0), Lit = Lex@LTag0}
  ;   {Lit = Lex^^xsd:string}
  ).



%! 'String'(-String:string)// is det.
% ```ebnf
% [66]   String ::= STRING_LITERAL1
%                 | STRING_LITERAL2
%                 | STRING_LITERAL_LONG1
%                 | STRING_LITERAL_LONG2
% ```

'String'(S) --> 'STRING_LITERAL1'(S),      !.
'String'(S) --> 'STRING_LITERAL2'(S),      !.
'String'(S) --> 'STRING_LITERAL_LONG1'(S), !.
'String'(S) --> 'STRING_LITERAL_LONG2'(S).



%! 'STRING_LITERAL_LONG1'(-String:string)// is det.
% A literal that can contain unescaped single quotes and newlines.
%
% ```ebnf
% [89]   STRING_LITERAL_LONG1 ::= "'''"
%                                 ( ( "'" | "''" )? ( [^'\] | ECHAR ) )*
%                                 "'''"
% ```
%
% This differs from Turtle 1.1 [24] in which escape sequences
% for Unicode characters are allowed.

'STRING_LITERAL_LONG1'(S) --> 'STRING_LITERAL_LONG'(single_quote, S).



%! 'STRING_LITERAL_LONG2'(-String:string)// is det.
% A literal that can contain unescaped single quotes and newlines.
%
% ```ebnf
% [90]   STRING_LITERAL_LONG2 ::= '"""'
%                                 ( ( '"' | '""' )? ( [^"\] | ECHAR ) )*
%                                 '"""'
% ```
%
% This differs from Turtle 1.1 [25] in which escape sequences
% for Unicode characters are allowed.

'STRING_LITERAL_LONG2'(S) --> 'STRING_LITERAL_LONG'(double_quote, S).



%! 'STRING_LITERAL1'(-String:string)// is det.
% ```ebnf
% [87]   STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
% ```
%
% @compat SPARQL 1.1 Query [156]
% This differs from Turtle 1.1 [23] in which escape sequences
% for Unicode characters are allowed.

'STRING_LITERAL1'(S) --> 'STRING_LITERAL'(single_quote, S).



%! 'STRING_LITERAL2'(-String:string)// is det.
% ```ebnf
% [88]   STRING_LITERAL2 ::= '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
% ```
%
% @compat SPARQL 1.1 Query [157]
% This differs from Turtle 1.1 [22] in which escape sequences
% for Unicode characters are explicitly allowed.

'STRING_LITERAL2'(S) --> 'STRING_LITERAL'(double_quote, S).



%! 'WS'// .
%! 'WS'(?Code:code)// .
% ```bnf
% WS ::= #x20 | #x9 | #xD | #xA
%        /* #x20=space #x9=character tabulation
%           #xD=carriage return #xA=new line */
% ```
%
% @compat SPARQL 1.0 [93].
% @compat SPARQL 1.1 Query [162].
% @compat Turtle 1.1 [161s].

'WS' --> [0x20].
'WS' --> [0x9].
'WS' --> [0xD].
'WS' --> [0xA].





% HELPERS %

pn_chars_dot(C)   --> 'PN_CHARS'(C).
pn_chars_dot(0'.) --> ".".


'STRING_LITERAL'(Q, S) -->
  quoted(Q, string_literal_codes(Q, Cs)),
  {string_codes(S, Cs)}.
string_literal_codes(Q, _)     --> Q,          !, {fail}.
string_literal_codes(_, _)     --> [0x5C],     !, {fail}.
string_literal_codes(_, _)     --> [0xA],      !, {fail}.
string_literal_codes(_, _)     --> [0xD],      !, {fail}.
string_literal_codes(Q, [H|T]) --> 'ECHAR'(H), !, string_literal_codes(Q, T).
string_literal_codes(Q, [H|T]) --> [H],        !, string_literal_codes(Q, T).
string_literal_codes(_, [])    --> "".


'STRING_LITERAL_LONG'(Q, S) -->
  quoted(3, Q, string_literal_long_codes(Q, Cs)),
  {string_codes(S, Cs)}.
string_literal_long_codes(_, _)     --> "\\",       !, {fail}.
string_literal_long_codes(Q, _)     --> Q, Q, Q,    !, {fail}.
string_literal_long_codes(Q, [H|T]) --> 'ECHAR'(H), !, string_literal_long_codes(Q, T).
string_literal_long_codes(Q, [H|T]) --> [H],        !, string_literal_long_codes(Q, T).
string_literal_long_codes(_, [])    --> "".
