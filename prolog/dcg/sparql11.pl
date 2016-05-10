:- module(
  sparql11,
  [
    'BLANK_NODE_LABEL'//1, % -BNodeLabel
    'HEX'//1,              % ?Weight:between(0,15)
    iri//1,                % -Iri
    'PERCENT'//1,          % ?C
    'PLX'//1,              % ?C
    'PN_LOCAL_ESC'//1      % ?C
  ]
).
:- reexport(library(dcg/sparql10), [
     'ANON'//1,                   % ?BNode
     'BlankNode'//1,              % ?BNode
     'BooleanLiteral'//1,         % ?Boolean
     'DECIMAL'//1,                % ?Decimal:rational
     'DECIMAL_NEGATIVE'//1,       % ?Decimal:rational
     'DECIMAL_POSITIVE'//1,       % ?Decimal:rational
     'DOUBLE'//1,                 % ?Double:float
     'DOUBLE_NEGATIVE'//1,        % ?Double:float
     'DOUBLE_POSITIVE'//1,        % ?Double:float
     'ECHAR'//1,                  % ?C
     'EXPONENT'//1,               % ?Exp:integer
     'INTEGER'//1,                % ?I:integer
     'INTEGER_NEGATIVE'//1,       % ?I:negative_integer
     'INTEGER_POSITIVE'//1,       % ?I:positive_integer
     'IRI_REF'//1 as 'IRIREF',    % ?Iri
     'IRIref'//1,                 % ?Iri
     'LANGTAG'//1,                % ?LTag:list(atom)
     'NumericLiteral'//1,         % ?Lit
     'NumericLiteralNegative'//1, % ?Lit
     'NumericLiteralPositive'//1, % ?Lit
     'NumericLiteralUnsigned'//1, % ?Lit
     'PN_CHARS'//1,               % ?C
     'PN_CHARS_U'//1,             % ?C
     'PNAME_LN'//1,               % ?Prefix
     'PNAME_NS'//1,               % ?Alias
     'PrefixedName'//1,           % -Iri
     'RDFLiteral'//1,             % ?Lit
     'String'//1,                 % ?String
     'STRING_LITERAL_LONG1'//1,   % ?String
     'STRING_LITERAL_LONG2'//1,   % ?String
     'STRING_LITERAL1'//1,        % ?String
     'STRING_LITERAL2'//1         % ?String
   ]).

/** <module> SPARQL 1.1: Tokens

@author Wouter Beek
@compat SPARQL 1.1
@see http://www.w3.org/TR/sparql11-query/#grammar
@version 2015/11-2016/01, 2016/05
*/

:- use_module(library(dcg/dcg_ext)).





%! 'BLANK_NODE_LABEL'(?BNodeLabel)// .
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

pn_chars_dot(C)   --> 'PN_CHARS'(C).
pn_chars_dot(0'.) --> ".".



%! 'HEX'(?Weight:between(0,15))// .
% ```abnf
% HEX ::= [0-9] | [A-F] | [a-f]
% ```

'HEX'(0)  --> "0".
'HEX'(1)  --> "1".
'HEX'(2)  --> "2".
'HEX'(3)  --> "3".
'HEX'(4)  --> "4".
'HEX'(5)  --> "5".
'HEX'(6)  --> "6".
'HEX'(7)  --> "7".
'HEX'(8)  --> "8".
'HEX'(9)  --> "9".
'HEX'(10) --> "a".
'HEX'(10) --> "a".
'HEX'(11) --> "B".
'HEX'(11) --> "b".
'HEX'(12) --> "C".
'HEX'(12) --> "c".
'HEX'(13) --> "D".
'HEX'(13) --> "d".
'HEX'(14) --> "E".
'HEX'(14) --> "e".
'HEX'(15) --> "F".
'HEX'(15) --> "f".



%! iri(-Iri)// is det.
% ```ebnf
% [136]   iri ::= IRIREF | PrefixedName
% ```

iri(A) --> 'IRIREF'(A), !.
iri(A) --> 'PrefixedName'(A).



%! 'PERCENT'(?Code:code)// .
% RFC 1738 (URL) defines the same thing under the name escape//1.
%
% ```bnf
% PERCENT ::= '%' HEX HEX
% ```

'PERCENT'(C) --> "%", 'HEX'(H1), 'HEX'(H2), {pos_sum([H1,H2], 16, C)}.



%! 'PLX'(?Code:code)// .
% ```bnf
% PLX ::= PERCENT | PN_LOCAL_ESC
% ```

'PLX'(C) --> 'PERCENT'(C).
'PLX'(C) --> 'PN_LOCAL_ESC'(C).



%! 'PN_LOCAL_ESC'(?Code:code)// .
% ```bnf
% PN_LOCAL_ESC ::= '\'
%                  ( '_' | '~' | '.' | '-' | '!' | '$' | '&' |
%                    "'" | '(' | ')' | * | '+' | ',' | ';' |
%                    '=' | '/' | '?' | '#' | '@' | '%'
%                  )
% ```

'PN_LOCAL_ESC'(C) --> "\\", pn_local_esc_code(C).
pn_local_esc_code(0'_) --> "_".
pn_local_esc_code(0'~) --> "~".
pn_local_esc_code(0'.) --> ".".
pn_local_esc_code(0'-) --> "-".
pn_local_esc_code(0'!) --> "!".
pn_local_esc_code(0'$) --> "$".
pn_local_esc_code(0'&) --> "&".
pn_local_esc_code(0'') --> "'".
pn_local_esc_code(0'() --> "(".
pn_local_esc_code(0')) --> ")".
pn_local_esc_code(0'*) --> "*".
pn_local_esc_code(0'+) --> "+".
pn_local_esc_code(0',) --> ",".
pn_local_esc_code(0';) --> ";".
pn_local_esc_code(0'=) --> "=".
pn_local_esc_code(0'/) --> "/".
pn_local_esc_code(0'?) --> "?".
pn_local_esc_code(0'#) --> "#".
pn_local_esc_code(0'@) --> "@".
pn_local_esc_code(0'%) --> "%".
