:- module(
  sparql11_code,
  [
  ]
).
:- reexport(library(dcg/sparql10_code), [
     'ECHAR'//1, % ?Code:code
     'WS'//0
   ]).

/** <module> SPARQL 1.1

@author Wouter Beek
@compat SPARQL 1.1
@see http://www.w3.org/TR/sparql11-query/#grammar
@version 2015/11
*/

:- use_module(library(dcg/dcg_re)).





%! 'PERCENT'(?Code:code)// .
% RFC 1738 (URL) defines the same thing under the name escape//1.
%
% ```bnf
% PERCENT ::= '%' HEX HEX
% ```

'PERCENT'(C) --> "%", 'HEX'(H1), 'HEX'(H2), {posnum([H1,H2], 16, C)}.



%! 'PLX'(?Code:code)// .
% ```bnf
% PLX ::= PERCENT | PN_LOCAL_ESC
% ```

'PLX'(C) --> 'PERCENT'(C).
'PLX'(C) --> 'PN_LOCAL_ESC'(C).
