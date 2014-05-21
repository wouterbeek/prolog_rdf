:- module(
  turtle_char,
  [
    'IRIREF'//1, % ?Value:float
    'UCHAR'//0
  ]
).
:- reexport(
  sparql_char,
  [
    'ANON'//0,
    'BLANK_NODE_LABEL'//0,
    'ECHAR'//0,
    'EXPONENT'//0,
    'HEX'//0,
    'LANGTAG'//0,
    'PERCENT'//0,
    'PLX'//0,
    'PN_CHARS'//0,
  ]
).

/** <module> Turtle character

DCGs for character definitions in Turtle recommendations.

@author Wouter Beek
@version 2014/04-2014/05
*/

:- use_module(dcg(dcg_content)).



%! 'IRIREF'// .
% ~~~{.ebfn}
% [18]   IRIREF ::= '<' ( [^#x00-#x20<>"{}|^`\] | UCHAR )* '>'
%                   /* #x00=NULL #01, '1F=control codes #x20=space */
% ~~~
%
% @compat Turtle 1.1 [18].

'IRIREF' --> bracketed(angular, 'IRIREF_char*').

'IRIREF_char*' --> 'IRIREF_char', 'IRIREF_char*'.
'IRIREF_char*' --> [].

'IRIREF_char' --> [C], {C =< 32}, !, {fail}.
'IRIREF_char' --> `<`, !, {fail}.
'IRIREF_char' --> `>`, !, {fail}.
'IRIREF_char' --> `"`, !, {fail}.
'IRIREF_char' --> `{`, !, {fail}.
'IRIREF_char' --> `}`, !, {fail}.
'IRIREF_char' --> `|`, !, {fail}.
'IRIREF_char' --> `^`, !, {fail}.
'IRIREF_char' --> `\``, !, {fail}.
'IRIREF_char' --> `\\`, !, {fail}.
'IRIREF_char' --> 'UCHAR'.
'IRIREF_char' --> [_].


%! 'UCHAR'// .
% ~~~{.ebnf}
% UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ~~~
%
% @compat Turtle 1.1 [26].

'UCHAR' --> `\\u`, 'HEX', 'HEX', 'HEX', 'HEX'.
'UCHAR' --> `\\U`, 'HEX', 'HEX', 'HEX', 'HEX', 'HEX', 'HEX', 'HEX', 'HEX'.

