:- module(
  rdf_guess_turtle,
  [
    rdf_guess_turtle//3 % +EOS:boolean, -Format:rdf_format, +Opts
  ]
).

/** <module> RDF guess: Turtle-family

@author Wouter Beek
@author Jan Wielemaker
@version 2015/12, 2016/02
*/

:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(option_ext)).
:- use_module(library(typecheck)).

:- meta_predicate
    turtle_string_codes(//,?,?).

:- predicate_options(rdf_guess_turtle//3, 3, [
     default_rdf_format(+turtle_format)
   ]).





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
  guess_turtle_family(F, Opts).
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
  guess_turtle_or_trig(F, Opts).
% BASE
% PREFIX
rdf_guess_turtle(_, F, Opts) -->
  turtle_keyword, blank, !,
  guess_turtle_or_trig(F, Opts).
% Turtle triple.
rdf_guess_turtle(_, F, Opts) -->
  turtle_subject(SF),
  *(ws),
  turtle_predicate(PF),
  *(ws),
  turtle_object(OF),
  *(ws),
  (   % End of a triple.
      "."
  ->  {
        exclude(var, [SF,PF,OF], Fs),
        guess_turtle_family(Fs, F, Opts)
      }
  ;   % Object list notation.
      ";"
  ->  guess_turtle_or_trig(F, Opts)
  ;   % Predicate-Object pairs list notation.
      ","
  ->  guess_turtle_or_trig(F, Opts)
  ;   % End of a quad.
      turtle_graph,
      *(ws),
      "."
  ->  {F = nquads}
  ).
% Anonymous blank node.
rdf_guess_turtle(_, F, Opts) -->
  "[", !,
  guess_turtle_or_trig(F, Opts).
% RDF collection.
rdf_guess_turtle(_, F, Opts) -->
  "(", !,
  guess_turtle_or_trig(F, Opts).

turtle_bnode --> "_:", *(nonblank).

turtle_graph --> turtle_iriref(_).

turtle_iriref(F) -->
  "<", !, ...(Cs), ">", !,
  {atom_codes(Iri, Cs), (is_iri(Iri) -> true ; F = turtleOrTrig)}.
turtle_iriref(_) -->
  turtle_iriref_prefix, ":", *(nonblank).

turtle_iriref_prefix --> ":",   !, {fail}.
turtle_iriref_prefix --> blank, !, {fail}.
turtle_iriref_prefix --> [_],      turtle_iriref_prefix.
turtle_iriref_prefix --> "".

turtle_ltag --> *(nonblank).

turtle_object(OF) --> turtle_iriref(OF), !.
turtle_object(_) --> turtle_bnode, !.
turtle_object(_) -->
  turtle_string, *(ws),
  ("^^" -> *(ws), turtle_iriref(_) ; "@" -> turtle_ltag ; "").

turtle_predicate(PF)      --> turtle_iriref(PF), !.
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

turtle_subject(SF) --> turtle_iriref(SF), !.
turtle_subject(_)       --> turtle_bnode.

ws      --> blank, !.
ws, " " --> "#", ..., (eol ; eos), !.

turtle_keyword --> atom_lower(A), !, {turtle_keyword(A)}.
turtle_keyword(base).
turtle_keyword(prefix).


%! guess_turtle_or_trig(-Format:turtle_format, +Opts)// is det.
% The file starts with a Turtle construct.
% It can still be TriG.
% We trust the content type and otherwise we assume TriG if there
% is a "{" in the first section of the file.

guess_turtle_or_trig(F, Opts) -->
  (   {
        option(default_rdf_format(F0), Opts),
        ground(F0)
      }
  ->  {
        must_be(oneof([trig,turtle]), F0),
        F = F0
      }
  ;   ..., "{"
  ->  {F = trig}
  ;   {F = turtle}
  ).


%! guess_turtle_family(
%!   +Formats:list(atom),
%!   -Format:turtle_format,
%!   +Opts
%! )// is det.
% We found a fully qualified triple.
% This still can be Turtle, TriG, N-Triples or N-Quads.

guess_turtle_family(Fs, F, Opts) :-
  memberchk(turtleOrTrig, Fs), !,
  (   option(default_rdf_format(F0), Opts),
      ground(F0)
  ->  must_be(oneof([trig,turtle]), F0),
      F = F0
  ;   F = turtle
  ).
guess_turtle_family(_, F, Opts) :-
  (   option(default_rdf_format(F0), Opts),
      ground(F0)
  ->  must_be(oneof([nquads,ntriples,trig,turtle]), F0),
      F = F0
  ;   F = ntriples
  ).
