:- module(
  ctriples_write_triples,
  [
    ctriples_write_triple/4, % +Write:stream
                             % +State:compound
                             % +BNodePrefix:iri
                             % +Triple:compound
    ctriples_write_triples/3, % +Write:or([atom,stream])
                              % +Triples:list(compound)
                              % +Options:list(compound)
    ctriples_write_triples_to_stream/2 % +Triples:list(compound)
                                       % +Options:list(compound)
  ]
).

/** <module> C-Triples write: Triples

Serialize RDF in the C-Triples and C-Quads format

A simple implementation for emitting RDF data in
C-Triples and C-Quads serialization formats,
reusing some of the Turtle writer.
Intended to work with RDF data stored using SWI-Prolog's Semweb library.

In C-Triples/C-Quads only short Turtle strings
(delimited by a single double quote) occur.
Linefeeds and carriage returns are escaped.
This means that we can guarantee that the number of triples
is the same as the number of lines in the generated file.

Also the number of spaces between the RDF terms is exactly one
and lines end in one space, one dot, and a newline character.

Simple literals are not emitted, the `xsd:string` datatype is made explicit.
Language-tagged strings are made explicit with datatype `rdf:langString`.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ctriples/ctriples_write_generics)).

:- predicate_options(ctriples_write_triples/3, 3, [
     pass_to(ctriples_write_triples_to_stream/2, 2)
   ]).
:- predicate_options(ctriples_write_triples_to_stream/2, 2, [
     pass_to(ctriples_write_begin/3, 3),
     pass_to(ctriples_write_end/2, 2)
   ]).





%! ctriples_write_triple(
%!   +Write:stream,
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   +Triple:compound
%! ) is det.

ctriples_write_triple(Write, State, BPrefix, T):-
  with_output_to(Write, ctriples_write_triple_to_stream(State, BPrefix, T)).



%! ctriples_write_triple_to_stream(
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   +Triple:compound
%! ) is det.

ctriples_write_triple_to_stream(State, BPrefix, T):-
  inc_number_of_triples(State),
  (   T = rdf(S,P,O)
  ->  write_triple(S, P, O, BPrefix)
  ;   T = rdf(S,P,O,G)
  ->  write_quadruple(S, P, O, G, BPrefix)
  ;   true
  ).



%! ctriples_write_triples(
%!   +Source:or([atom,stream]),
%!   +Triples:list(compound),
%!   +Options:list(compound)
%! ) is det.
% Writes RDF data using the C-Triples/C-Quads serialization format.

ctriples_write_triples(Write, Ts, Opts):-
  is_stream(Write), !,
  with_output_to(Write, ctriples_write_triples_to_stream(Ts, Opts)).
ctriples_write_triples(Write, Ts, Opts):-
  is_absolute_file_name(File),
  setup_call_cleanup(
    open(File, write, Write),
    with_output_to(Write, ctriples_write_triples_to_stream(Ts, Opts)),
    close(Write)
  ).



%! ctriples_write_triples_to_stream(
%!   +Triples:list(compound),
%!   +Options:list(compound)
%! ) is det.

ctriples_write_triples_to_stream(Ts1, Opts):-
  ctriples_write_begin(State, BPrefix, Opts),
  sort(Ts1, Ts2),
  maplist(ctriples_write_triple_to_stream(State, BPrefix), Ts2),
  ctriples_write_end(State, Opts).
