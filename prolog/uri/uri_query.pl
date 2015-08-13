:- module(
  uri_query,
  [
    query//2 % ?Iri:boolean
             % ?Query:atom
  ]
).

:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(uri/uri_char)).





%! query(?Iri:boolean, ?Query:atom)// .
% ```abnf
%  query = *(  pchar /            "/" / "?" )
% iquery = *( ipchar / iprivate / "/" / "?" )
% ```
%
% @compat RFC 3986
% @compat RFC 3987

query(I, Query) -->
  dcg_atom(query_codes(I), Query).

query_codes(I, [H|T]) -->
  (   pchar(I, H)
  ;   {I = true},
      private(H)
  ;   forward_slash(H)
  ;   question_mark(H)
  ),
  query_codes(I, T).
query_codes(_, []) --> [].
