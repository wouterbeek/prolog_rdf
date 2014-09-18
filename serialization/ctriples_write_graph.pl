:- module(
  ctriples_write_graph,
  [
    ctriples_write_graph/3 % +Write:or([atom,stream])
                           % ?Graph:atom
                           % +Options:list(nvpair)
  ]
).

/** <module> C-Triples write: graph

Writes the given graph (or all currently stored triples) to a source.

@author Wouter Beek
@version 2014/03-2014/06, 2014/08-2014/09
*/

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf_ser(ctriples_write_generics)).

:- predicate_options(ctriples_write_graph/3, 3, [
     pass_to(write_graph/2, 2)
   ]).
:- predicate_options(write_graph/2, 2, [
     bnode_base(+atom),
     format(+oneof([quads,triples])),
     number_of_triples(-nonneg)
   ]).



%! ctriples_write_graph(
%!   +Source:or([atom,stream]),
%!   ?Graph:atom,
%!   +Options:list(nvpair)
%! ) is det.
% `Source` is either a file name or a write stream.
%
% The following options are supported:
%   * =|bnode_base(+atom)|=
%     Replace blank nodes with an IRI, defined as per
%     RDF 1.1 spec (see link below).
%   * =|format(-oneof([quads,triples]))|=
%     `quads` if at least one named graph occurs, `triples` otherwise.
%   * =|number_of_triples(-nonneg)|=
%     The number of triples that was written.
%
% @see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-skolemization

% Input is a stream.
ctriples_write_graph(Write, Graph, Options):-
  is_stream(Write), !,
  with_output_to(Write, write_graph(Graph, Options)).
% Input is a file.
% Open a stream in `write` mode.
ctriples_write_graph(File, Graph, Options):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, write, Write),
    with_output_to(Write, write_graph(Graph, Options)),
    close(Write)
  ).


%! write_graph(?Graph:atom, +Options:list(nvpair)) is det.
% This assumes that we can write to current output.

write_graph(Graph, Options):-
  ctriples_write_begin(State, BNodePrefix, Options),
  
  % Decide whether triples or quadruples are written.
  (   option(format(Format), Options),
      nonvar(Format)
  ->  memberchk(Format, [quads,triples])
  ;   rdf(_, _, _, G),
      G \== user
  ->  Format = quads
  ;   Format = triples
  ),

  findall(
    S,
    rdf(S, _, _, Graph),
    Ss1
  ),
  sort(Ss1, Ss2),
  forall(
    member(S, Ss2),
    write_subject(State, BNodePrefix, Graph, Format, S)
  ),

  ctriples_write_end(Options, State).


%! write_subject(
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   ?Graph:atom,
%!   +Format:or([quads,triples]),
%!   +Subject:or([bnode,iri])
%! ) is det.
% Writes all triples that occur with the given subject term,
% possibly restricted to a given graph.
%
% Collects a sorted list of predicate-object pairs.
% Then processes each pairs -- and thus each triple -- separately.

% Format: C-Quads
write_subject(State, BNodePrefix, Graph, quads, S):-
  findall(
    P-O-Graph,
    rdf(S, P, O, Graph),
    POGTriples1
  ),
  sort(POGTriples1, POGTriples2),
  forall(
    member(P-O-G, POGTriples2),
    (
      inc_number_of_triples(State),
      write_quad(S, P, O, G, BNodePrefix)
    )
  ).
% Format: C-Triples
write_subject(State, BNodePrefix, Graph, triples, S):-
  findall(
    P-O,
    rdf(S, P, O, Graph),
    POPairs1
  ),
  sort(POPairs1, POPairs2),
  forall(
    member(P-O, POPairs2),
    (
      inc_number_of_triples(State),
      write_triple(S, P, O, BNodePrefix)
    )
  ).

