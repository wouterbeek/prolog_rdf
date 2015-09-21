:- module(
  sw_ltag_char,
  [
    alphanum//1, % ?Code:code
    singleton//1 % ?Code:code
  ]
).

/** <module> Characters for Language Tags grammar.

Language subtags are case-insensitive.

@author Wouter Beek
@compat RFC 5646
@version 2015/08-2015/09
*/

:- use_module(library(dcg/dcg_abnf_rules)).
:- use_module(library(dcg/dcg_code)).





%! alphanum(?Code:code)// .
% ```abnf
% alphanum = (ALPHA / DIGIT)
% ```
%
% Case-insensitive.
%
% @compat RFC 5646

alphanum(C) --> 'ALPHA'(C).
alphanum(C) --> 'DIGIT'(_, C).



%! singleton(?Code:code)// .
% Single alphanumeric.
%
% ```abnf
% singleton =   DIGIT     ; 0 - 9
%             / %x41-57   ; A - W
%             / %x59-5A   ; Y - Z
%             / %x61-77   ; a - w
%             / %x79-7A   ; y - z
% ```
%
% Case-sensitive.
%
% `x` is reserved for private use.
%
% @compat RFC 5646

singleton(C) --> 'DIGIT'(_, C).
singleton(C) -->  between_code_radix(hex('41'), hex('57'), C).
singleton(C) -->  between_code_radix(hex('59'), hex('5A'), C).
singleton(C) -->  between_code_radix(hex('61'), hex('77'), C).
singleton(C) -->  between_code_radix(hex('79'), hex('7A'), C).

