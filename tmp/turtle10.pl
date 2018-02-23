:- module(
  turtle10,
  [
    name//1, % -Name:string
    nameChar//1, % ?Code:code
    nameStartChar//1, % ?Code:code
    nodeID//1 % -BlankNode:bnode
  ]
).

/** <module> Turtle 1.0

@author Wouter Beek
@compat Turtle 1,0
@deprecated
@version 2015/11, 2016/01
*/

:- use_module(library(dcg)).
:- use_module(library(dcg/rfc2234)).





%! name(-Name:string)// is det.
% ```ebnf
% name ::= nameStartChar nameChar*
% ```

name(S) -->
  nameStartChar(H),
  *(nameChar, T),
  {string_codes(S, [H|T])}.



%! nameChar(?Code:code)// .
% ```ebnf
% nameChar ::= nameStartChar
%            | '-'
%            | [0-9]
%            | #x00B7
%            | [#x0300-#x036F]
%            | [#x203F-#x2040]
% ```

nameChar(Code) --> nameStartChar(Code).
nameChar(0'-) --> "-".
nameChar(Code) --> 'DIGIT'(Code).
nameChar(0x00B7) --> [0x00B7].
nameChar(Code) --> between(0x0300, 0x036F, Code).
nameChar(Code) --> between(0x203F, 0x2040, Code).



%! nameStartChar(?Code:code)// .
% ```ebnf
% nameStartChar ::= [A-Z]
%                 | "_"
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

nameStartChar(Code) --> alpha(Code).
nameStartChar(0'_) --> "_".
nameStartChar(Code) --> between(0x00C0,  0x00D6,  Code).
nameStartChar(Code) --> between(0x00D8,  0x00F6,  Code).
nameStartChar(Code) --> between(0x00F8,  0x02FF,  Code).
nameStartChar(Code) --> between(0x0370,  0x037D,  Code).
nameStartChar(Code) --> between(0x037F,  0x1FFF,  Code).
nameStartChar(Code) --> between(0x200C,  0x200D,  Code).
nameStartChar(Code) --> between(0x2070,  0x218F,  Code).
nameStartChar(Code) --> between(0x2C00,  0x2FEF,  Code).
nameStartChar(Code) --> between(0x3001,  0xD7FF,  Code).
nameStartChar(Code) --> between(0xF900,  0xFDCF,  Code).
nameStartChar(Code) --> between(0xFDF0,  0xFFFD,  Code).
nameStartChar(Code) --> between(0x10000, 0xEFFFF, Code).



%! nodeID(?BlankNode:bnode)// .
% ```bnf
% nodeID ::= '_:' name
% ```

nodeID(BNodeLabel) -->
  "_:",
  name(BNodeLabel).
