:- module(
  rdf_guess_jsonld,
  [
    rdf_guess_jsonld/2 % +S, +N
  ]
).

/** <module> RDF guess JSON-LD

@author Wouter Beek
@version 2016/03, 2016/06, 2016/12
*/

:- use_module(library(dcg/dcg_ext)).





rdf_guess_jsonld(S, N) :-
  % Do not backtrack if the whole stream has been peeked.
  string_length(S, M),
  ((M =:= 0 ; M < N) -> !, true ; true),
  string_phrase(rdf_guess_jsonld_grammar, S, _).

rdf_guess_jsonld_grammar -->
  *(bs),
  optional_arrays,
  "{",
  *(bs),
  jsonld_string,
  *(bs),
  ":", !.

optional_arrays -->
  "[", !,
  *(bs),
  optional_arrays.
optional_arrays --> "".

% Start of jsonld_string.
jsonld_string --> "\"", jsonld_string0.

% Escaped double quote.
jsonld_string0 --> "\\\"", !, jsonld_string0.
% End of jsonld_string.
jsonld_string0 --> "\"", !.
% Middle of jsonld_string.
jsonld_string0 --> [_], !, jsonld_string0.
