:- module(
  uri_fragment,
  [
    fragment//2 % ?Iri:boolean
                % ?Fragment:atom
  ]
).

/** <module> URI Syntax: Fragment

Grammar for URI fragments.

@author Wouter Beek
@compat RFC 3986
@see tools.ietf.org/html/rfc3986 
@version 2015/08
*/

:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(uri/uri_char)).





%! fragment(?Iri:boolean, ?Fragment:atom)// .
% ```anbf
%  fragment = *(  pchar / "/" / "?" )
% ifragment = *( ipchar / "/" / "?" )
% ```
%
% @compat RFC 3986
% @compat RFC 3987

fragment(I, Fragment) -->
  dcg_atom(fragment_codes(I), Fragment).

fragment_codes(I, [H|T]) -->
  (   pchar(I, H)
  ;   forward_slash(H)
  ;   question_mark(H)
  ),
  fragment_codes(I, T).
fragment_codes(_, []) --> [].
