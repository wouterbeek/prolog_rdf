:- module(
  rdf_guess_turtle,
  [
    rdf_guess_turtle//3 % +EndOfStream:boolean
                        % -Format:rdf_format
                        % +Options:list(compound)
  ]
).

/** <module> RDF guess: Turtle-family

@author Wouter Beek
@author Jan Wielemaker
@version 2015/12
*/

:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(option_ext)).
:- use_module(library(typecheck)).

:- meta_predicate(turtle_string_codes(//,?,?)).

:- predicate_options(rdf_guess_turtle//3, 3, [
     default_format(+rdf_format)
   ]).





%! rdf_guess_turtle(
%!   +EoS:boolean,
%!   -Format:rdf_format,
%!   +Options:list(compound)
%! )// is semidet.
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
rdf_guess_turtle(true, Format, Opts) -->
  eos, !,
  guess_turtle_or_trig(Format, Opts).
% Skip blanks.
rdf_guess_turtle(EoS, Format, Opts) -->
  blank, !, blanks,
  rdf_guess_turtle(EoS, Format, Opts).
% Turtle comment.
rdf_guess_turtle(EoS, Format, Opts) -->
  "#", !, skip_line,
  rdf_guess_turtle(EoS, Format, Opts).
% @BASE
% @PREFIX
rdf_guess_turtle(_, Format, Opts) -->
  "@", turtle_keyword, !,
  guess_turtle_or_trig(Format, Opts).
% BASE
% PREFIX
rdf_guess_turtle(_, Format, Opts) -->
  turtle_keyword, blank, !,
  guess_turtle_or_trig(Format, Opts).
% Turtle triple.
rdf_guess_turtle(_, Format, Opts) -->
  turtle_subject(SFormat),
  *('WS'),
  turtle_predicate(PFormat),
  *('WS'),
  turtle_object(OFormat),
  *('WS'),
  (   % End of triple.
      "."
  ->  {
        exclude(var, [SFormat,PFormat,OFormat], Formats),
        guess_turtle_family(Formats, Format, Opts)
      }
  ;   % Object list notation.
      ";"
  ->  guess_turtle_or_trig(Format, Opts)
  ;   % Predicate-Object pairs list notation.
      ","
  ->  guess_turtle_or_trig(Format, Opts)
  ;   % End of quadruple.
      turtle_graph,
      *('WS'),
      "."
  ->  {Format = nquads}
  ).
% Anonymous blank node.
rdf_guess_turtle(_, Format, Opts) -->
  "[", !,
  guess_turtle_or_trig(Format, Opts).
% RDF collection.
rdf_guess_turtle(_, Format, Opts) -->
  "(", !,
  guess_turtle_or_trig(Format, Opts).

turtle_bnode --> "_:", *(nonblank).

turtle_graph --> turtle_iriref(_).

turtle_iriref(Format) -->
  "<", !, ...(Cs), ">", !,
  {atom_codes(Iri, Cs), (is_iri(Iri) -> true ; Format = turtleOrTrig)}.
turtle_iriref(_) -->
  turtle_iriref_prefix, ":", *(nonblank).

turtle_iriref_prefix --> ":",   !, {fail}.
turtle_iriref_prefix --> blank, !, {fail}.
turtle_iriref_prefix --> [_],      turtle_iriref_prefix.
turtle_iriref_prefix --> "".

turtle_ltag --> *(nonblank).

turtle_object(OFormat) --> turtle_iriref(OFormat), !.
turtle_object(_) --> turtle_bnode, !.
turtle_object(_) -->
  turtle_string, *('WS'),
  ("^^" -> *('WS'), turtle_iriref(_) ; "@" -> turtle_ltag ; "").

turtle_predicate(PFormat)      --> turtle_iriref(PFormat), !.
turtle_predicate(turtleOrTrig) --> "a".

% Triple single quotes.
turtle_string --> "'''",    !, turtle_string_codes([0'',0'',0'']).
% Single single quotes.
turtle_string --> "'",      !, turtle_string_codes([0'']).
% Triple double quotes.
turtle_string --> "\"\"\"", !, turtle_string_codes([0'",0'",0'"]).   %"
% Single double quotes.
turtle_string --> "\"",        turtle_string_codes([0'"]).   %"

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

turtle_subject(SFormat) --> turtle_iriref(SFormat), !.
turtle_subject(_)       --> turtle_bnode.

'WS'      --> blank, !.
'WS', " " --> "#", ..., (eol ; eos), !.

turtle_keyword --> atom_lower(A), !, {turtle_keyword(A)}.
turtle_keyword(base).
turtle_keyword(prefix).


%! guess_turtle_or_trig(-Format:rdf_format, +Options:list(compound))// is det.
% The file starts with a Turtle construct.
% It can still be TriG.
% We trust the content type and otherwise we assume TriG if there
% is a "{" in the first section of the file.

guess_turtle_or_trig(Format, Opts) -->
  (  {option(default_format(Format0), Opts),
      ground(Format0)}
  -> {must_be(oneof([trig,turtle]), Format0),
      Format = Format0}
  ;   ..., "{"
  -> {Format = trig}
  ;  {Format = turtle}
  ).


%! guess_turtle_family(
%!   +Formats:list(oneof([nquads,ntriples,trig,turtle])),
%!   -Format:rdf_format,
%!   +Options:list(compound)
%! )// is det.
% We found a fully qualified triple.
% This still can be Turtle, TriG, N-Triples or N-Quads.

guess_turtle_family(Formats, Format, Opts):-
  memberchk(turtleOrTrig, Formats), !,
  (   option(default_format(Format0), Opts),
      ground(Format0)
  ->  must_be(oneof([trig,turtle]), Format0),
      Format = Format0
  ;   debug(rdf(guess), 'Assuming Turtle based on heuristics.', []),
      Format = turtle
  ).
guess_turtle_family(_, Format, Opts):-
  (   option(default_format(Format0), Opts),
      ground(Format0)
  ->  must_be(oneof([nquads,ntriples,trig,turtle]), Format0),
      Format = Format0
  ;   debug(rdf(guess), 'Assuming N-Triples based on heuristics.', []),
      Format = ntriples
  ).
