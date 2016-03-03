:- module(
  rdf_guess_jsonld,
  [
    rdf_guess_jsonld/2 % +S, +N
  ]
).

/** <module> RDF guess JSON-LD

@author Wouter Beek
@version 2016/03
*/

:- use_module(library(dcg/dcg_ext)).





rdf_guess_jsonld(S, N) :-
  % Do not backtrack if the whole stream has been peeked.
  string_length(S, M),
  ((M =:= 0 ; M < N) -> !, EoS = true ; EoS = false),
  string_phrase(rdf_guess_jsonld(EoS), S, _).

% End of stream reached before we could determine whether this is JSON-LD.
rdf_guess_jsonld(true) -->
  eos, !, {fail}.
% Empty JSON object.
rdf_guess_jsonld(true) -->
  "{", *(ws), "}", !.
% Empty JSON array.
rdf_guess_jsonld(true) -->
  "[", *(ws), "]", !.
% Skip blanks.
rdf_guess_jsonld(EoS) -->
  blank, !, blanks,
  rdf_guess_jsonld(EoS).
% First object member pair.
rdf_guess_jsonld(_) -->
  "{", *(ws), jsonld_string, *(ws), ":", *(ws), jsonld_value, !.
% First array element value.
rdf_guess_jsonld(_) -->
  "[", *(ws), jsonld_value, !.

jsonld_value --> jsonld_string.
jsonld_value --> number, !.
jsonld_value --> "true", !.
jsonld_value --> "false", !.
jsonld_value --> "null", !.
jsonld_value --> "{", !.
jsonld_value --> "[", !.

% Start of jsonld_string.
jsonld_string --> "\"", jsonld_string0.

% Escaped double quote.
jsonld_string0 --> "\\\"", !, jsonld_string0.
% End of jsonld_string.
jsonld_string0 --> "\"", !.
% Middle of jsonld_string.
jsonld_string0 --> [_], !, jsonld_string0.

ws --> blank.
ws --> eol.
ws --> eos.
