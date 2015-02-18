:- module(
  ctriples_write_triples,
  [
    ctriples_write_triple/3, % +Out:stream
                             % +BNodePrefix:iri
                             % +Triple:compound
    ctriples_write_triple/4, % +Out:stream
                             % +State:compound
                             % +BNodePrefix:iri
                             % +Triple:compound
    ctriples_write_triples/3, % +Out:or([atom,stream])
                              % +Triples:list(compound)
                              % +Options:list(nvpair)
    ctriples_write_triples_to_stream/2 % +Triples:list(compound)
                                       % +Options:list(nvpair)
  ]
).

/** <module> C-Triples write: triples

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
@version 2014/08-2014/09, 2015/01-2015/02
*/

:- use_module(library(lists), except([delete/3])).

:- use_module(plRdf(syntax/ctriples/ctriples_write_generics)).

:- predicate_options(ctriples_write_triples/3, 3, [
     pass_to(ctriples_write_triples_to_stream/2, 2)
   ]).
:- predicate_options(ctriples_write_triples_to_stream/2, 2, [
     pass_to(ctriples_write_begin/3, 3),
     pass_to(ctriples_write_end/2, 2)
   ]).





%! ctriples_write_triple(
%!   +Out:stream,
%!   +BNodePrefix:iri,
%!   +Triple:compound
%! ) is det.

ctriples_write_triple(Out, BNodePrefix, Triple):-
  with_output_to(Out, ctriples_write_triple0(BNodePrefix, Triple)).

%! ctriples_write_triple(
%!   +Out:stream,
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   +Triple:compound
%! ) is det.

ctriples_write_triple(Out, State, BNodePrefix, Triple):-
  with_output_to(Out, ctriples_write_triple0(State, BNodePrefix, Triple)).

%! ctriples_write_triple0(+BNodePrefix:iri, +Triple:compound) is det.

ctriples_write_triple0(BNodePrefix, Triple):-
  (   Triple = rdf(S,P,O)
  ->  write_triple(S, P, O, BNodePrefix)
  ;   Triple = rdf(S,P,O,G)
  ->  write_quadruple(S, P, O, G, BNodePrefix)
  ;   true
  ).

%! ctriples_write_triple0(
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   +Triple:compound
%! ) is det.

ctriples_write_triple0(State, BNodePrefix, Triple):-
  inc_number_of_triples(State),
  ctriples_write_triple0(BNodePrefix, Triple).



%! ctriples_write_triples(
%!   +Source:or([atom,stream]),
%!   +Triples:list(compound),
%!   +Options:list
%! ) is det.
% Writes RDF data using the C-Triples/C-Quads serialization format.

ctriples_write_triples(Out, Triples, Options):-
  is_stream(Out), !,
  with_output_to(Out, ctriples_write_triples_to_stream(Triples, Options)).
ctriples_write_triples(Out, Triples, Options):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, write, Out),
    with_output_to(Out, ctriples_write_triples_to_stream(Triples, Options)),
    close(Out)
  ).



%! ctriples_write_triples_to_stream(
%!   +Triples:list(compound),
%!   +Options:list(nvpair)
%! ) is det.

ctriples_write_triples_to_stream(Triples1, Options):-
  ctriples_write_begin(State, BNodePrefix, Options),

  sort(Triples1, Triples2),
  forall(
    member(Triple, Triples2),
    ctriples_write_triple(State, BNodePrefix, Triple)
  ),

  ctriples_write_end(State, Options).
