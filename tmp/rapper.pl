:- module(
  rapper,
  [
    rapper/2 % +File, +Opts
  ]
).

/** <module> Rapper

Rapper support.

@author Wouter Beek
@see http://librdf.org/raptor/rapper.html
@version 2015/03
*/

:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(os/process_ext)).
:- use_module(library(readutil)).

:- predicate_options(rapper/2, 2, [
     count(+boolean),
     guess(+boolean),
     ignore_errors(+boolean),
     input(+oneof([ntriples,rdfxml,'rss-tag-soup',turtle]))
   ]).





%! rapper(+File, +Opts) is det.
% The following options are supported:
%   - count(+boolean)
%     Only count the triples and produce no other output.
%     Default is `false`.
%   - guess(+boolean)
%     Guess the parser to use from the source rather than use
%     the input/1 option to determine the RDF serialization format.
%     Default is `false`.
%   - ignore_errors(+boolean)
%     Ignore errors, do not emit the messages and try to continue parsing.
%     Default is `false`.
%   - input(+oneof([ntriples,rdfxml,'rss-tag-soup',turtle]))
%     The RDF serialization format of the input.
%   - triples(-nonneg)
%     The number of parsed triples.
%
% @tbd Why is all _output_ send to the standard _error_ stream?

rapper(File, _) :- var(File), !, instantiation_error(File).
rapper(File, _) :- \+ exists_file(File), !, existence_error(file, File).
rapper(File, _) :- \+ access_file(File, read), !, permission_error(read, file, File).
rapper(File, Opts) :-
  rapper_args(Opts, Args),
  handle_process(
    rapper,
    [file(File)|Args],
    [error_goal(rapper_result(Ts)),program(rapper)]
  ),
  ignore(option(triples(Ts), Opts)).

rapper_args([], []) :- !.
rapper_args([count(true)|T1], ['--count'|T2]) :- !, rapper_args(T1, T2).
rapper_args([guess(true)|T1], ['--guess'|T2]) :- !, rapper_args(T1, T2).
rapper_args([ignore_errors(true)|T1], ['--ignore-errors'|T2]) :- !,
  rapper_args(T1, T2).
rapper_args([input(Format)|T1], [Arg|T2]) :- !,
  format(atom(Arg), '--~input=~w', [Format]),
  rapper_args(T1, T2).
rapper_args([_|T1], T2) :- rapper_args(T1, T2).

rapper_result(Ts, Out) :-
  read_stream_to_codes(Out, Cs),
  atom_codes(A, Cs), writeln(A), %DEB
  phrase(rapper_result(Ts), Cs).

rapper_result(Ts) -->
  "rapper: Parsing returned ", integer(Ts), " ", ev(triple, Ts), "\n".
rapper_result(Ts) --> ..., "\n", !, rapper_result(Ts).

ev(Word, 1) --> !, atom(Word).
ev(Word, _) --> atom(Word), "s".
