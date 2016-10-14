:- module(
  xsd_boolean,
  [
     booleanCanonicalMap//1, % +Boolean:boolean
     booleanLexicalMap//1 % -Boolean:boolean
  ]
).

/** <module> XSD: boolean

**`Boolean`** represents the values of two-valued logic.

% ```ebnf
% booleanRep ::= 'true' | 'false' | '1' | '0'
% ```

@author Wouter Beek
@compat XSD 1.1
@version 2015/07, 2015/11
*/





%! booleanCanonicalMap(+Boolean:boolean)// .
% Maps a boolean value to a booleanRep//1.
%
% Return:
%   - `'true'`, when `b` is true.
%   - `'false'`, otherwise (i.e., `b` is false).

booleanCanonicalMap(true)  --> !, "true".
booleanCanonicalMap(false) -->    "false".



%! booleanLexicalMap(-Boolean:boolean)// .
% Maps a literal matching the booleanRep//1 production to a boolean value.
%
% Return:
%   - `true`, when `LEX` is `'true'` or `'1'`.
%   - `false`, otherwise (i.e., `LEX` is `'false'` or `'0'`).

booleanLexicalMap(true)  --> "true",  !.
booleanLexicalMap(true)  --> "1",     !.
booleanLexicalMap(false) --> "false", !.
booleanLexicalMap(false) --> "0".
