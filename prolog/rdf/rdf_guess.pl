:- module(
  rdf_guess,
  [
    rdf_guess_format/2,      % +In, -Format
    rdf_guess_format/3,      % +In, -Format, +Opts
    rdf_guess_format_file/2, % +File, -Format
    rdf_guess_format_file/3  % +File, -Format, +Opts
  ]
).

/** <module> RDF guess

@author Wouter Beek
@author Jan Wielemaker
@version 2015/08-2015/12, 2016/02-2016/04
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(os/io)).
:- use_module(library(rdf/rdf_guess_jsonld)).
:- use_module(library(rdf/rdf_guess_turtle)).
:- use_module(library(rdf/rdf_guess_xml)).
:- use_module(library(yall)).





%! rdf_guess_format(+In, -Format) is det.
%! rdf_guess_format(+In, -Format, +Opts) is det.
% The following options are supported:
%   * default_rdf_format(+rdf_format)

rdf_guess_format(In, Format) :-
  rdf_guess_format(In, Format, []).


rdf_guess_format(In, Format, Opts) :-
  rdf_guess_format0(In, 0, Format, Opts).


rdf_guess_format0(In, I, Format, Opts) :-
  N is 1000 * 2 ^ I,
  peek_string(In, N, S),
  debug(rdf(guess), "[RDF-GUESS] ~s", [S]),

  % Try to parse the peeked string as Turtle- or XML-like.
  (   rdf_guess_jsonld(S, N),
      Format = jsonld
  ;   rdf_guess_turtle(S, N, Format, Opts)
  ;   rdf_guess_xml(S, Format)
  ), !,

  debug(rdf(guess), "Assuming ~a based on heuristics.", [Format]).
rdf_guess_format0(In, I1, Format, Opts) :-
  I1 < 4,
  I2 is I1 + 1,
  rdf_guess_format0(In, I2, Format, Opts).


% For Turtle-family formats it matters whether or not
% end of stream has been reached.
rdf_guess_turtle(S, N, Format, Opts) :-
  % Do not backtrack if the whole stream has been peeked.
  string_length(S, M),
  ((M =:= 0 ; M < N) -> !, EoS = true ; EoS = false),
  string_phrase(rdf_guess_turtle(EoS, Format, Opts), S, _).




%! rdf_guess_format_file(+File, -Format) is det.
%! rdf_guess_format_file(+File, -Format, +Opts) is det.

rdf_guess_format_file(File, Format) :-
  rdf_guess_format_file(File, Format, []).


rdf_guess_format_file(File, Format, Opts) :-
  call_on_stream(
    File,
    [In,Meta,Meta]>>rdf_guess_format(In, Format, Opts),
    Opts
  ).
