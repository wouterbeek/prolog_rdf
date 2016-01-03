:- module(
  sparql11_code,
  [
    'PERCENT'//1, % ?Code:code
    'PLX'//1, % ?Code:code
    'PN_LOCAL_ESC'//1 % ?Code:code
  ]
).
:- reexport(library(dcg/sparql10_code), [
     'ECHAR'//1, % ?Code:code
     'PN_CHARS_U'//1, % ?Code:code
     'WS'//0
   ]).

/** <module> SPARQL 1.1

@author Wouter Beek
@compat SPARQL 1.1
@see http://www.w3.org/TR/sparql11-query/#grammar
@version 2015/11-2016/01
*/

:- use_module(library(dcg/dcg_ext)).





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
