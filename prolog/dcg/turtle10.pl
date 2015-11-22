:- module(
  turtle10,
  [
    name//1, % ?Name:string
    nameChar//1, % ?Code:code
    nameStartChar//1, % ?Code:code
    nodeID//1 % ?BlankNode:bnode
  ]
).

/** <module> Turtle 1.0

@author Wouter Beek
@compat Turtle 1,0
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_re)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234)).





%! name(?Name:string)// .
% ```ebnf
% name ::= nameStartChar nameChar*
% ```

name(Name) --> dcg_string(name_codes, S).
name_codes([H|T]) --> nameStartChar(H), *(nameChar, T).



%! nameChar(?Code:code)// .
% ```ebnf
% nameChar ::= nameStartChar
%            | '-'
%            | [0-9]
%            | #x00B7
%            | [#x0300-#x036F]
%            | [#x203F-#x2040]
% ```

nameChar(C)   --> nameStartChar(C).
nameChar(0'-) --> "-".
nameChar(C)   --> 'DIGIT'(C).
nameChar(183) --> [183].
nameChar(C)   --> [C], {(between(768, 879, C)}.
nameChar(C)   --> [C], {(between(8255, 8256, C)}.



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

nameStartChar(C)   --> ascii_upper(C).
nameStartChar(0'_) --> "_".
nameStartChar(C)   --> ascii_lower(C).
nameStartChar(C)   --> [C], {between(0x00C0,  0x00D6,  C)}.
nameStartChar(C)   --> [C], {between(0x00D8,  0x00F6,  C)}.
nameStartChar(C)   --> [C], {between(0x00F8,  0x02FF,  C)}.
nameStartChar(C)   --> [C], {between(0x0370,  0x037D,  C)}.
nameStartChar(C)   --> [C], {between(0x037F,  0x1FFF,  C)}.
nameStartChar(C)   --> [C], {between(0x200C,  0x200D,  C)}.
nameStartChar(C)   --> [C], {between(0x2070,  0x218F,  C)}.
nameStartChar(C)   --> [C], {between(0x2C00,  0x2FEF,  C)}.
nameStartChar(C)   --> [C], {between(0x3001,  0xD7FF,  C)}.
nameStartChar(C)   --> [C], {between(0xF900,  0xFDCF,  C)}.
nameStartChar(C)   --> [C], {between(0xFDF0,  0xFFFD,  C)}.
nameStartChar(C)   --> [C], {between(0x10000, 0xEFFFF, C)}.



%! nodeID(?BlankNode:bnode)// .
% ```bnf
% nodeID ::= '_:' name
% ```

nodeID(BNodeLabel) --> "_:", name(BNodeLabel).
