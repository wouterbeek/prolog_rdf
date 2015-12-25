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
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).





%! 'HEX'(?Weight:between(0,15))// .
% ```abnf
% HEX ::= [0-9] | [A-F] | [a-f]
% ```



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
