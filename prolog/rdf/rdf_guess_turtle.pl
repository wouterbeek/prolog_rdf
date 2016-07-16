:- module(
  rdf_guess_turtle,
  [
    rdf_guess_turtle//3 % +EOS:boolean, -Format:rdf_format, +Opts
  ]
).

/** <module> RDF guess: Turtle-family

@author Wouter Beek
@author Jan Wielemaker
@version 2015/12, 2016/02-2016/03, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(option_ext)).
:- use_module(library(ordsets)).
:- use_module(library(typecheck)).

:- meta_predicate
    turtle_string_codes(//, ?, ?).





%! rdf_guess_turtle(+EoS:boolean, -Format:rdf_format, +Opts)// is semidet.
% True if the start of the input matches a turtle-like language.
% There are four of them:
%   1. Turtle
%   2. TRiG
%   3. ntriples
%   4. nquads.
%
% The first three can all be handled by the turtle parser,
% so it oesn't matter too much.

% Whenever the end-of-stream is reached we assume it is in a format
% belonging to the Turtle family.
% This e.g. allows empty files or files that only consist of Turtle comments
% to be classified as such.
% as well).
rdf_guess_turtle(true, F, Opts) -->
  eos, !,
  {guess_turtle_format([], F, Opts)}.
% Skip blanks.
rdf_guess_turtle(EoS, F, Opts) -->
  blank, !, blanks,
  rdf_guess_turtle(EoS, F, Opts).
% Turtle comment.
rdf_guess_turtle(EoS, F, Opts) -->
  "#", !, skip_line,
  rdf_guess_turtle(EoS, F, Opts).
% @BASE
% @PREFIX
rdf_guess_turtle(_, F, Opts) -->
  "@", turtle_keyword, !,
  {guess_turtle_format([nquads,ntriples], F, Opts)}.
% BASE
% PREFIX
rdf_guess_turtle(_, F, Opts) -->
  turtle_keyword, blank, !,
  {guess_turtle_format([nquads,ntriples], F, Opts)}.
% Default graph.
rdf_guess_turtle(_, F, Opts) -->
  "{", !,
  {guess_turtle_format([trig], F, Opts)}.
% Named graph.
rdf_guess_turtle(_, F, Opts) -->
  turtle_iriref(_), *(bs), "{", !,
  {guess_turtle_format([trig], F, Opts)}.
% Tuple.
rdf_guess_turtle(_, F, Opts) -->
  turtle_subject(Fs1),
  *(bs),
  turtle_predicate(Fs2),
  *(bs),
  turtle_object(Fs3),
  *(bs),
  (   % End of a triple.
      "."
  ->  {ord_union([Fs1,Fs2,Fs3], Fs)}
  ;   % Object list notation.
      ";"
  ->  {ord_union([Fs1,Fs2,Fs3,[nquads,ntriples]], Fs)}
  ;   % Predicate-Object pairs list notation.
      ","
  ->  {ord_union([Fs1,Fs2,Fs3,[nquads,ntriples]], Fs)}
  ;   % End of a quad.
      turtle_graph(Fs4),
      *(bs),
      "."
  ->  {ord_union([Fs1,Fs2,Fs3,Fs4,[ntriples,trig,turtle]], Fs)}
  ),
  {guess_turtle_format(Fs, F, Opts)}.
% Anonymous blank node.
rdf_guess_turtle(_, F, Opts) -->
  "[", !,
  {guess_turtle_format([nquads,ntriples], F, Opts)}.
% RDF collection.
rdf_guess_turtle(_, F, Opts) -->
  "(", !,
  {guess_turtle_format([nquads,ntriples], F, Opts)}.

turtle_bnode --> "_:", *(nonblank).

turtle_graph(Fs) --> turtle_iriref(Fs).

turtle_iriref([]) --> "<", !, ..., ">", !.
turtle_iriref([nquads,ntriples]) --> turtle_iriref_prefix, ":", *(nonblank).

turtle_iriref_prefix --> ":", !, {fail}.
turtle_iriref_prefix --> blank, !, {fail}.
turtle_iriref_prefix --> [_], turtle_iriref_prefix.
turtle_iriref_prefix --> "".

turtle_ltag --> *(nonblank).

turtle_object(Fs) --> turtle_iriref(Fs), !.
turtle_object([]) --> turtle_bnode, !.
turtle_object(Fs) -->
  turtle_string(Fs1), *(bs),
  (   "^^"
  ->  *(bs), turtle_iriref(Fs2)
  ;   "@"
  ->  turtle_ltag, {Fs2 = []}
  ;   {Fs2 = []}
  ),
  {ord_union([Fs1,Fs2], Fs)}.

turtle_predicate(Fs) --> turtle_iriref(Fs), !.
turtle_predicate([nquads,ntriples]) --> "a".

% Triple single quotes.
turtle_string([nquads,ntriples]) --> "'''", !, turtle_string_codes([0'',0'',0'']).
% Single single quotes.
turtle_string([nquads,ntriples]) --> "'", !, turtle_string_codes([0'']).
% Triple double quotes.
turtle_string([nquads,ntriples]) --> "\"\"\"", !, turtle_string_codes([0'",0'",0'"]). %"
% Single double quotes.
turtle_string([]) --> "\"", turtle_string_codes([0'"]). %"

% Escaped single quote.
turtle_string_codes(End) --> "\\\'", !, turtle_string_codes(End).
% Escaped double quote.
turtle_string_codes(End) --> "\\\"", !, turtle_string_codes(End).
% End of string.
turtle_string_codes(End) --> End,    !.
% Content.
turtle_string_codes(End) --> [_],    !, turtle_string_codes(End).
% End of stream.
turtle_string_codes(_)   --> "".

turtle_subject(Fs) --> turtle_iriref(Fs), !.
turtle_subject([]) --> turtle_bnode.

bs0 --> bs, !.
bs0, " " --> "#", ..., (eol ; eos), !.

turtle_keyword --> "base".
turtle_keyword --> "prefix".

%! guess_turtle_format(
%!   +Excluded:ordset(rdf_format),
%!   -Format:turtle_format,
%!   +Opts
%! )// is det.
% We found a fully qualified triple.  Determine whether it is
% Turtle, TriG, N-Triples or N-Quads.

guess_turtle_format(Excluded, F, Opts) :-
  ord_subtract([nquads,ntriples,trig,turtle], Excluded, Fs),
  (   % Narrowed down to one.
      Fs = [F]
  ->  true
  ;   % Multiple options: pick the one specified as an option, if any.
      option(default_rdf_format(F0), Opts),
      ground(F0),
      memberchk(F0, Fs)
  ->  F = F0
  ;   % Multiple options: Trig is more general than Turtle.
      F = trig,
      memberchk(F, Fs)
  ;   % Multiple options: N-Quads is more general than N-Triples.
      F = nquads,
      memberchk(F, Fs)
  ).
