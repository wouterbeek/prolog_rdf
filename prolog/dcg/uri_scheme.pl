:- module(
  uri_scheme,
  [
    scheme//1 % ?Scheme:atom
  ]
).

/** <module> URI Syntax: Scheme

Grammar for URI schemes.

@author Wouter Beek
@compat RFC 3987
@see [Guidelines and Registration Procedures for New URI Schemes](https://tools.ietf.org/html/rfc4395)
@see [IANA URI Scheme Register](http://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml)
@see [RFC 3987](http://tools.ietf.org/html/rfc3987)
@version 2015/08
*/

:- use_module(library(dcg/dcg_abnf_rules)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_word)).





%! scheme(?Scheme:atom)// .
% An US-ASCII letter, followed by a sequence consisting of
% US-ASCII letters, digits, plus, dash, and dot characters.
%
% ```abnf
% scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
% ```
%
% @compat RFC 3986
% @compat RFC 3987

scheme(Scheme) -->
  dcg_atom(scheme_codes1, Scheme),
  {memberchk(Scheme, [ftp,http,https,mailto])}.

scheme_codes1([H|T]) -->
  'ALPHA'(H),
  scheme_codes2(T).

scheme_codes2([H|T]) -->
  (   'ALPHA'(H)
  ;   'DIGIT'(_, H)
  ;   plus_sign(H)
  ;   minus_sign(H)
  ;   dot(H)
  ),
  scheme_codes2(T).
scheme_codes2([]) --> [].
