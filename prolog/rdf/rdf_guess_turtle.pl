:- module(
  rdf_guess_turtle,
  [
    rdf_guess_turtle//3 % +EOS:boolean, -MT, +Opts
  ]
).

/** <module> RDF guess: Turtle-family

@author Wouter Beek
@author Jan Wielemaker
@version 2015/12, 2016/02-2016/03, 2016/06, 2016/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(option_ext)).
:- use_module(library(ordsets)).
:- use_module(library(typecheck)).

:- meta_predicate
    turtle_string_codes(//, ?, ?).





%! rdf_guess_turtle(+EoS:boolean, -MT, +Opts)// is semidet.
%
% True if the start of the input matches a Turtle-like language.
%
% These are Turtle-like Media Types:
%
%   1. application/turtle
%
%   2. application/trig
%
%   3. application/'n-triples'
%
%   4. application/'n-quads'
%
% The first three can all be handled by the turtle parser,
% so it oesn't matter too much.

rdf_guess_turtle(EoS, application/Subtype, Opts) -->
  rdf_guess_turtle_subtype(EoS, Subtype, Opts).

% Whenever the end-of-stream is reached we assume it is in a format
% belonging to the Turtle family.  This e.g. allows empty files or
% files that only consist of Turtle comments to be classified as
% Turtle-family files as well.
rdf_guess_turtle_subtype(true, Subtype, Opts) -->
  eos, !,
  {guess_turtle_subtype([], Subtype, Opts)}.
% Skip blanks.
rdf_guess_turtle_subtype(EoS, Subtype, Opts) -->
  blank, !, blanks,
  rdf_guess_turtle_subtype(EoS, Subtype, Opts).
% Turtle comment.
rdf_guess_turtle_subtype(EoS, Subtype, Opts) -->
  "#", !, skip_line,
  rdf_guess_turtle_subtype(EoS, Subtype, Opts).
% @BASE
% @PREFIX
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  "@", turtle_keyword, !,
  {guess_turtle_subtype(['n-quads','n-triples'], Subtype, Opts)}.
% BASE
% PREFIX
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  turtle_keyword, blank, !,
  {guess_turtle_subtype(['n-quads','n-triples'], Subtype, Opts)}.
% Default graph.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  "{", !,
  {guess_turtle_subtype([trig], Subtype, Opts)}.
% Named graph.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  turtle_iriref(_), *(bs), "{", !,
  {guess_turtle_subtype([trig], Subtype, Opts)}.
% Tuple.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  turtle_subject(Subtypes1),
  *(bs),
  turtle_predicate(Subtypes2),
  *(bs),
  turtle_object(Subtypes3),
  *(bs),
  (   % End of a triple.
      "."
  ->  {ord_union([Subtypes1,Subtypes2,Subtypes3], Subtypes)}
  ;   % Object list notation.
      ";"
  ->  {
        ord_union(
          [Subtypes1,Subtypes2,Subtypes3,['n-quads','n-triples']],
          Subtypes
        )
      }
  ;   % Predicate-Object pairs list notation.
      ","
  ->  {
        ord_union(
          [Subtypes1,Subtypes2,Subtypes3,['n-quads','n-triples']],
          Subtypes
        )
      }
  ;   % End of a quad.
      turtle_graph(Subtypes4),
      *(bs),
      "."
  ->  {
        ord_union(
          [Subtypes1,Subtypes2,Subtypes3,Subtypes4,['n-triples',trig,turtle]],
          Subtypes
        )
      }
  ),
  {guess_turtle_subtype(Subtypes, Subtype, Opts)}.
% Anonymous blank node.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  "[", !,
  {guess_turtle_subtype(['n-quads','n-triples'], Subtype, Opts)}.
% RDF collection.
rdf_guess_turtle_subtype(_, Subtype, Opts) -->
  "(", !,
  {guess_turtle_subtype(['n-quads','n-triples'], Subtype, Opts)}.

turtle_bnode --> "_:", *(nonblank).

turtle_graph(Subtypes) --> turtle_iriref(Subtypes).

turtle_iriref([]) -->
  "<", !, ..., ">", !.
turtle_iriref(['n-quads','n-triples']) -->
  turtle_iriref_prefix, ":", *(nonblank).

turtle_iriref_prefix --> ":", !, {fail}.
turtle_iriref_prefix --> blank, !, {fail}.
turtle_iriref_prefix --> [_], turtle_iriref_prefix.
turtle_iriref_prefix --> "".

turtle_ltag --> *(nonblank).

turtle_object(Subtypes) --> turtle_iriref(Subtypes), !.
turtle_object([]) --> turtle_bnode, !.
turtle_object(Subtypes) -->
  turtle_string(Subtypes1), *(bs),
  (   "^^"
  ->  *(bs),
      turtle_iriref(Subtypes2)
  ;   "@"
  ->  turtle_ltag,
      {Subtypes2 = []}
  ;   {Subtypes2 = []}
  ),
  {ord_union([Subtypes1,Subtypes2], Subtypes)}.

turtle_predicate(Subtypes) --> turtle_iriref(Subtypes), !.
turtle_predicate(['n-quads','n-triples']) --> "a".

% Triple single quotes.
turtle_string(['n-quads','n-triples']) -->
  "'''", !,
  turtle_string_codes([0'',0'',0'']).
% Single single quotes.
turtle_string(['n-quads','n-triples']) -->
  "'", !,
  turtle_string_codes([0'']).
% Triple double quotes.
turtle_string(['n-quads','n-triples']) -->
  "\"\"\"", !,
  turtle_string_codes([0'",0'",0'"]). %"
% Single double quotes.
turtle_string([]) -->
  "\"",
  turtle_string_codes([0'"]). %"

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

turtle_subject(Subtypes) --> turtle_iriref(Subtypes), !.
turtle_subject([]) --> turtle_bnode.

bs0 --> bs, !.
bs0, " " --> "#", ..., (eol ; eos), !.

turtle_keyword --> "base".
turtle_keyword --> "prefix".

%! guess_turtle_subtype(+Excluded:ordset, -Subtype, +Opts)// is det.
%
% We found a fully qualified triple.  Determine whether it is Turtle,
% TriG, N-Triples or N-Quads.

guess_turtle_subtype(Excluded, Subtype, Opts) :-
  ord_subtract(['n-quads','n-triples',trig,turtle], Excluded, Subtypes),
  (   % Narrowed down to one.
      Subtypes = [Subtype]
  ->  true
  ;   % Multiple options: pick the one specified as an option, if any.
      option(default_rdf_media_type(application/Subtype0), Opts),
      ground(Subtype0),
      memberchk(Subtype0, Subtypes)
  ->  Subtype = Subtype0
  ;   % Multiple options: Trig is more general than Turtle.
      Subtype = trig,
      memberchk(Subtype, Subtypes)
  ;   % Multiple options: N-Quads is more general than N-Triples.
      Subtype = 'n-quads',
      memberchk(Subtype, Subtypes)
  ).
