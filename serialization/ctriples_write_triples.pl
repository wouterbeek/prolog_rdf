:- module(
  ctriples_write_triples,
  [
    ctriples_write_triples/3, % +Write:or([atom,stream])
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
@author Jan Wielemaker
@author Laurens Rietveld
@compat http://www.w3.org/TR/2014/REC-n-triples-20140225/
@version 2014/03-2014/06, 2014/08-2014/09
*/

:- use_module(library(lists)).

:- use_module(plRdf_ser(ctriples_write_generics)).



%! ctriples_write_triples(
%!   +Source:or([atom,stream]),
%!   +Triples:list(compound),
%!   +Options:list
%! ) is det.
% Writes RDF data using the C-Triples/C-Quads serialization format.

ctriples_write_triples(Write, Triples, Options):-
  is_stream(Write), !,
  with_output_to(Write, ctriples_write_triples_to_stream(Triples, Options)).
ctriples_write_triples(Write, Triples, Options):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, write, Write),
    with_output_to(Write, ctriples_write_triples_to_stream(Triples, Options)),
    close(Write)
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


%! ctriples_write_triple(
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   +Triple:list(compound)
%! ) is det.

ctriples_write_triple(State, BNodePrefix, Triple):-
  inc_number_of_triples(State),
  (   Triple = rdf(S,P,O)
  ->  write_triple(S, P, O, BNodePrefix)
  ;   Triple = rdf(S,P,O,G)
  ->  write_quad(S, P, O, G, BNodePrefix)
  ;   true
  ).


