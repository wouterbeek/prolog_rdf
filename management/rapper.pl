:- module(
  rapper,
  [
    rapper/2 % +File:atom
             % +Options:list(nvpair)
  ]
).

/** <module> Rapper

Rapper support.

@author Wouter Beek
@see http://librdf.org/raptor/rapper.html
@version 2015/03
*/

:- use_module(library(dcg/basics)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(readutil)).

:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(os/cli_ext)).
:- use_module(plc(process/process_ext)).

:- predicate_options(rapper/2, 2, [
  count(+boolean),
  guess(+boolean),
  ignore_errors(+boolean),
  input(+oneof([ntriples,rdfxml,'rss-tag-soup',turtle]))
]).





%! rapper(+File:atom, +Options:list(nvpair)) is det.
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

rapper(File, _):-
  var(File), !,
  instantiation_error(File).
rapper(File, _):-
  \+ exists_file(File), !,
  existence_error(file, File).
rapper(File, _):-
  \+ access_file(File, read), !,
  permission_error(read, file, File).
rapper(File, Options):-
  rapper_args(Options, Args),
  handle_process(
    rapper,
    [file(File)|Args],
    [output_goal(rapper_result(Triples)),program(rapper)]
  ),
  ignore(option(triples(Triples), Options)).

rapper_args([], []).
rapper_args([count(true)|T1], ['--count'|T2]):-
  rapper_args(T1, T2).
rapper_args([guess(true)|T1], ['--guess'|T2]):-
  rapper_args(T1, T2).
rapper_args([ignore_errors(true)|T1], ['--ignore-errors'|T2]):-
  rapper_args(T1, T2).
rapper_args([input(Format)|T1], [Arg|T2]):-
  cli_long_flag(input, Format, Arg),
  rapper_args(T1, T2).
rapper_args([_|T1], T2):-
  rapper_args(T1, T2).

rapper_result(Triples, Out):-
  read_stream_to_codes(Out, Codes),
  atom_codes(Atom, Codes), %DEB
  writeln(Atom), %DEB
  phrase(rapper_result(Triples), Codes).

rapper_result(Triples) -->
  "rapper: Parsing returned ",
  integer(Triples),
  " ",
  ev(triple, Triples),
  "\n".
rapper_result(Triples) -->
  '...',
  "\n", !,
  rapper_result(Triples).

ev(Word, 1) --> !,
  atom(Word).
ev(Word, _) -->
  atom(Word),
  "s".

