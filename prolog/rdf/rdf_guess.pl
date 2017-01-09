:- module(
  rdf_guess,
  [
    rdf_guess_media_type/2, % +Source, -MT
    rdf_guess_media_type/3  % +Source, -MT, +Opts
  ]
).

/** <module> RDF guess

@author Wouter Beek
@author Jan Wielemaker
@version 2015/08-2015/12, 2016/02-2016/04, 2016/11, 2017/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(os/io)).
:- use_module(library(rdf/rdf_guess_jsonld)).
:- use_module(library(rdf/rdf_guess_turtle)).
:- use_module(library(rdf/rdf_guess_xml)).
:- use_module(library(yall)).





%! rdf_guess_media_type(+Source, -MT) is det.
%! rdf_guess_media_type(+Source, -MT, +Opts) is det.
%
% The following options are supported:
%
%   * default_rdf_media_type(+rdf_media_type)

rdf_guess_media_type(Source, MT) :-
  rdf_guess_media_type(Source, MT, []).


rdf_guess_media_type(Source, MT, Opts) :-
  call_on_stream(
    Source,
    {MT,Opts}/[In,InPath,InPath]>>rdf_guess_media_type_stream(In, 0, MT, Opts)
  ).

rdf_guess_media_type_stream(In, I, MT, Opts) :-
  N is 1000 * 2 ^ I,
  peek_string(In, N, S),
  debug(rdf(guess), "[RDF-GUESS] ~s", [S]),
  % Try to parse the peeked string as Turtle- or XML-family.
  (   rdf_guess_jsonld(S, N),
      MT = application/'ld+json'
  ;   rdf_guess_turtle(S, N, MT, Opts)
  ;   rdf_guess_xml(S, MT)
  ), !,
  debug(rdf(guess), "Assuming ~a based on heuristics.", [MT]).
rdf_guess_media_type_stream(In, I1, MT, Opts) :-
  I1 < 4,
  I2 is I1 + 1,
  rdf_guess_media_type_stream(In, I2, MT, Opts).

% For Turtle-family media_types it matters whether or not the end of
% stream has been reached.
rdf_guess_turtle(S, N, MT, Opts) :-
  % Do not backtrack if the whole stream has been peeked.
  string_length(S, M),
  ((M =:= 0 ; M < N) -> !, EoS = true ; EoS = false),
  string_phrase(rdf_guess_turtle(EoS, MT, Opts), S, _).
