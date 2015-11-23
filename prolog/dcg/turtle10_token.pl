:- module(
  turtle10_token,
  [
    name//1, % ?Name:string
    nodeID//1 % ?BlankNode:bnode
  ]
).

/** <module> Turtle 1.0: Tokens

@author Wouter Beek
@compat Turtle 1,0
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234)).





%! name(?Name:string)// .
% ```ebnf
% name ::= nameStartChar nameChar*
% ```

name(S) --> dcg_string(name_codes, S).
name_codes([H|T]) --> nameStartChar(H), *(nameChar, T).



%! nodeID(?BlankNode:bnode)// .
% ```bnf
% nodeID ::= '_:' name
% ```

nodeID(BNodeLabel) --> "_:", name(BNodeLabel).
