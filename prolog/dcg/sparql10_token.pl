:- module(
  sparql10_token,
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
    'STRING_LITERAL2'//1 % ?String:atom
  ]
).

/** <module> SPARQL 1.0

@author Wouter Beek
@compat SPARQL 1.0
@deprecated
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_quote)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(math/rational_ext)).
:- use_module(library(rdf/rdf_datatype)).

:- rdf_meta('NumericLiteral'(o,?,?)).
:- rdf_meta('NumericLiteralNegative'(o,?,?)).
:- rdf_meta('NumericLiteralPositive'(o,?,?)).
:- rdf_meta('NumericLiteralUnsigned'(o,?,?)).





%! 'ANON'(-BlankNode:bnode)// is det.
% A blank node that is used in only one place in the query syntax
% can be indicated with the notation `[]`.
%
% A unique blank node will be used to form the triple pattern.
%
% ```ebnf
% [94]   ANON ::= '[' WS* ']'
% ```

'ANON'(BNode) --> "[", *('WS'), "]", {rdf_bnode(BNode)}.



%! 'BLANK_NODE_LABEL'(?BlankNodeLabel:atom)// .
% ```ebnf
% [73]   BLANK_NODE_LABEL ::= '_:' PN_LOCAL
% ```

'BLANK_NODE_LABEL'(A) --> "_:", 'PN_LOCAL'(A).



%! 'BlankNode'(?BlankNode:bnode)// .
% Blank nodes are indicated by either the label form,
% such as `_:abc`, or the abbreviated form `[]`.
%
% ```ebnf
% [69]   BlankNode ::= BLANK_NODE_LABEL | ANON
% ```

'BlankNode'(BNode) --> 'BLANK_NODE_LABEL'(BNode), !.
'BlankNode'(BNode) --> 'ANON'(BNode).



%! 'BooleanLiteral'(?Value:boolean)// is det.
% As a convenience, values of type `xsd:boolean`
% can also be written as `true` or `false`
% (without quotation marks and an explicit datatype IRI).
%
% ```ebnf
% [65]   BooleanLiteral ::= 'true' | 'false'
% ```

'BooleanLiteral'(literal(type(xsd:boolean,false))) --> "false", !.
'BooleanLiteral'(literal(type(xsd:boolean,true)))  --> "true".



%! 'DECIMAL'(?Decimal:rational)// .
% ```ebnf
% [78]   DECIMAL ::= [0-9]* '.' [0-9]+
% ```

'DECIMAL'(Rat) -->
  '*digit'(I),
  ".",
  +(digit, Ds2), {posfrac(Ds2, Frac)},
  {rational_parts(Rat, I, Frac)}.



%! 'DECIMAL_NEGATIVE'(?Decimal:rational)// .
% ```bnf
% [84]   DECIMAL_NEGATIVE ::= '-' DECIMAL
% ```

'DECIMAL_NEGATIVE'(Rat) --> "-", 'DECIMAL'(Rat0), {Rat is -Rat0}.



%! 'DECIMAL_POSITIVE'(?Decimal:rational)// .
% ```bnf
% [81]   DECIMAL_POSITIVE ::= '+' INTEGER
% ```

'DECIMAL_POSITIVE'(Rat) --> "+", 'INTEGER'(Rat).



%! 'DOUBLE'(?Double:float)// .
% ```ebnf
% [79]   DOUBLE ::= [0-9]+ '.' [0-9]* EXPONENT
%                 | '.' [0-9]+ EXPONENT
%                 | [0-9]+ EXPONENT
% ```
%
% @compat Turtle 1.1 [21]

'DOUBLE'(F) -->
  '+digit'(I), !,
  ".", *(digit, Ds2), {posfrac(Ds2, Frac)},
  'EXPONENT'(Exp), {F is float(I + Frac) * 10 ^ Exp}.
'DOUBLE'(F) -->
  ".", !, +(digit, Ds), {posfrac(Ds, Frac)},
  'EXPONENT'(Exp), {F is float(Frac) * 10 ^ Exp}.
'DOUBLE'(F) -->
  '+digit'(I),
  'EXPONENT'(Exp), {F is float(I) * 10 ^ Exp}.



%! 'DOUBLE_NEGATIVE'(?Double:float)// .
% ```bnf
% [85]   DOUBLE_NEGATIVE ::= '-' DOUBLE
% ```

'DOUBLE_NEGATIVE'(F) --> "-", 'DOUBLE'(F0), {F is -F0}.



%! 'DOUBLE_POSITIVE'(?Double:float)// .
% ```bnf
% [82]   DOUBLE_POSITIVE ::= '+' DOUBLE
% ```

'DOUBLE_POSITIVE'(F) --> "+", 'DOUBLE'(F).



%! 'EXPONENT'(?Exponent:integer)// .
% ```ebnf
% [86]   EXPONENT ::= [eE] [+-]? [0-9]+
% ```
%
% @compat SPARQL 1.0 [86]
% @compat SPARQL 1.1 Query [155]
% @compat Turtle 1.1 [154s]

'EXPONENT'(Exp) -->
  ("e", ! ; "E"),
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1} ; {Sg = 1}),
  '+digit'(Exp0),
  {Exp is Sg * Exp0}.



%! 'INTEGER'(?Integer:integer)// .
% ```ebnf
% [77]   INTEGER ::= [0-9]+
% ```

'INTEGER'(I) --> {var(I)}, !, '+digit'(I).
'INTEGER'(I) --> {pos(I, Ds)}, +(digit, Ds).



%! 'INTEGER_NEGATIVE'(?Integer:negative_integer)// .
% ```bnf
% [83]   INTEGER_NEGATIVE ::= '-' INTEGER
% ```

'INTEGER_NEGATIVE'(I) --> "-", 'INTEGER'(I0), {I is -I0}.



%! 'INTEGER_POSITIVE'(?Integer:positive_integer)// .
% ```bnf
% [80]   INTEGER_POSITIVE ::= '+' INTEGER
% ```

'INTEGER_POSITIVE'(I) --> "+", 'INTEGER'(I).



%! 'IRI_REF'(?Iri:atom)// .
% ```ebnf
% [70]   IRI_REF ::=  '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
% ```
%
% @deprecated Use 'IRIREF'//1 instead.

'IRI_REF'(Iri) --> 'IRIREF'(Iri).



%! 'IRIref'(?Iri:atom)// .
% ```ebnf
% [67]   IRIref ::= IRI_REF | PrefixedName
% ```
%
% @deprecated Use iri//1 instead.

'IRIref'(Iri) --> 'IRI_REF'(Iri), !.
'IRIref'(Iri) --> 'PrefixedName'(Iri).



%! 'LANGTAG'(?LanguageTag:list(atom))// .
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



%! 'NumericLiteral'(?Literal:compound)// .
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



%! 'NumericLiteralNegative'(?Literal:compound)// is det.
% ```ebnf
% [64]   NumericLiteralNegative ::= INTEGER_NEGATIVE
%                                 | DECIMAL_NEGATIVE
%                                 | DOUBLE_NEGATIVE
% ```

'NumericLiteralNegative'(literal(type(xsd:integer,CLex))) -->
  'INTEGER_NEGATIVE'(N), !,
  {rdf_canonical_map(xsd:integer, N, CLex)}.
'NumericLiteralNegative'(literal(type(xsd:decimal,CLex))) -->
  'DECIMAL_NEGATIVE'(N), !,
  {rdf_canonical_map(xsd:decimal, N, CLex)}.
'NumericLiteralNegative'(literal(type(xsd:double,CLex))) -->
  'DOUBLE_NEGATIVE'(N),
  {rdf_canonical_map(xsd:double, N, CLex)}.



%! 'NumericLiteralPositive'(?Literal:compound)// is det.
% ```ebnf
% [63]   NumericLiteralPositive ::= INTEGER_POSITIVE
%                                 | DECIMAL_POSITIVE
%                                 | DOUBLE_POSITIVE
% ```

'NumericLiteralPositive'(literal(type(xsd:integer,CLex))) -->
  'INTEGER_POSITIVE'(N), !,
  {rdf_canonical_map(xsd:decimal, N, CLex)}.
'NumericLiteralPositive'(literal(type(xsd:decimal,CLex))) -->
  'DECIMAL_POSITIVE'(N), !,
  {rdf_canonical_map(xsd:decimal, N, CLex)}.
'NumericLiteralPositive'(literal(type(xsd:double,CLex))) -->
  'DOUBLE_POSITIVE'(N),
  {rdf_canonical_map(xsd:decimal, N, CLex)}.



%! 'NumericLiteralUnsigned'(?Value:number)// is det.
% ```ebnf
% [62]   NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
% ```

'NumericLiteralUnsigned'(literal(type(xsd:integer,CLex))) -->
  'INTEGER'(N), !,
  {rdf_canonical_map(xsd:decimal, N, CLex)}.
'NumericLiteralUnsigned'(literal(type(xsd:decimal,CLex))) -->
  'DECIMAL'(N), !,
  {rdf_canonical_map(xsd:decimal, N, CLex)}.
'NumericLiteralUnsigned'(literal(type(xsd:double,CLex))) -->
  'DOUBLE'(N),
  {rdf_canonical_map(xsd:decimal, N, CLex)}.



%! 'PN_LOCAL'(?LocalPart:atom)// is det.
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



%! 'PN_PREFIX'(?Prefix:atom)// .
% ```ebnf
% [99]   PN_PREFIX ::= PN_CHARS_BASE ( ( PN_CHARS | '.' )* PN_CHARS )?
% ```

'PN_PREFIX'(A) -->
  'PN_CHARS_BASE'(H),
  (*(pn_chars_dot, T), 'PN_CHARS'(X) -> {append([H|T], [X], Cs)} ; {Cs = [H]}),
  {atom_codes(A, Cs)}.



%! 'PNAME_LN'(?Iri:atom)// .
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



%! 'PrefixedName'(?Iri:atom)// is det.
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



%! 'RDFLiteral'(?Literal:compound)// is det.
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
  ->  'IRIref'(D), {Lit = literal(type(D,Lex))}
  ;   'LANGTAG'(LTag),
      {atomic_list_concat(LTag, -, LTag0), Lit = literal(lang(LTag0,Lex))}
  ;   {Lit = literal(type(xsd:string,Lex))}
  ).



%! 'String'(?String:atom)// .
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



%! 'STRING_LITERAL_LONG1'(?String:atom)// .
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



%! 'STRING_LITERAL_LONG2'(?String:atom)// .
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



%! 'STRING_LITERAL1'(?String:atom)// .
% ```ebnf
% [87]   STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
% ```
%
% @compat SPARQL 1.1 Query [156]
% This differs from Turtle 1.1 [23] in which escape sequences
% for Unicode characters are allowed.

'STRING_LITERAL1'(S) --> 'STRING_LITERAL'(single_quote, S).



%! 'STRING_LITERAL2'(?String:atom)// .
% ```ebnf
% [88]   STRING_LITERAL2 ::= '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
% ```
%
% @compat SPARQL 1.1 Query [157]
% This differs from Turtle 1.1 [22] in which escape sequences
% for Unicode characters are explicitly allowed.

'STRING_LITERAL2'(S) --> 'STRING_LITERAL'(double_quote, S).





% HELPERS %

pn_chars_dot(C)   --> 'PN_CHARS'(C).
pn_chars_dot(0'.) --> ".".


'STRING_LITERAL'(Q, A) --> quoted(Q, dcg_atom(string_literal_codes(Q), A)).
string_literal_codes(Q, _)     --> Q,          !, {fail}.
string_literal_codes(_, _)     --> [0x5C],     !, {fail}.
string_literal_codes(_, _)     --> [0xA],      !, {fail}.
string_literal_codes(_, _)     --> [0xD],      !, {fail}.
string_literal_codes(Q, [H|T]) --> 'ECHAR'(H), !, string_literal_codes(Q, T).
string_literal_codes(Q, [H|T]) --> [H],        !, string_literal_codes(Q, T).
string_literal_codes(_, [])    --> "".


'STRING_LITERAL_LONG'(Q, A) --> quoted(3, Q, dcg_atom(string_literal_long_codes(Q), A)).
string_literal_long_codes(_, _)     --> "\\",       !, {fail}.
string_literal_long_codes(Q, _)     --> Q, Q, Q,    !, {fail}.
string_literal_long_codes(Q, [H|T]) --> 'ECHAR'(H), !, string_literal_long_codes(Q, T).
string_literal_long_codes(Q, [H|T]) --> [H],        !, string_literal_long_codes(Q, T).
string_literal_long_codes(_, [])    --> "".
