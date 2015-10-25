:- module(
  ctriples_write_graph,
  [
    ctriples_write_graph/3 % +Write:or([atom,stream])
                           % ?Graph:atom
                           % +Options:list(compound)
  ]
).

/** <module> C-Triples write: graph

Writes the given graph (or all currently stored triples) to a source.

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).

:- use_module(library(ctriples/ctriples_write_generics)).

:- predicate_options(ctriples_write_graph/3, 3, [
     pass_to(write_graph/2, 2)
   ]).
:- predicate_options(write_graph/2, 2, [
     pass_to(ctriples_write_begin/3, 3),
     pass_to(ctriples_write_end/2, 2),
     format(+oneof([quadruples,triples]))
   ]).





%! ctriples_write_graph(
%!   +Source:or([atom,stream]),
%!   ?Graph:atom,
%!   +Options:list(compound)
%! ) is det.
% `Source` is either a file name or a write stream.
%
% The following options are supported:
%   - `bnode_base(+atom)`
%     Replace blank nodes with an IRI, defined as per
%     RDF 1.1 spec (see link below).
%   - `format(-oneof([quadruples,triples]))`
%     `quadruples` if at least one named graph occurs, `triples` otherwise.
%   - `number_of_triples(-nonneg)`
%     The number of triples that was written.
%
% @see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-skolemization

% Input is a stream.
ctriples_write_graph(Write, G, Opts):-
  is_stream(Write), !,
  with_output_to(Write, write_graph(G, Opts)).
% Input is a file.
% Open a stream in `write` mode.
ctriples_write_graph(File, G, Opts):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, write, Write),
    with_output_to(Write, write_graph(G, Opts)),
    close(Write)
  ).



%! write_graph(?Graph:atom, +Options:list(compound)) is det.
% This assumes that we can write to current output.

write_graph(G, Opts):-
  ctriples_write_begin(State, BPrefix, Opts),

  % Decide whether triples or quadruples are written.
  (   option(format(CFormat), Opts),
      nonvar(CFormat)
  ->  memberchk(CFormat, [quadruples,triples])
  ;   rdf_graph(G),
      G \== user,
      rdf_db:rdf(_, _, _, G:_)
  ->  CFormat = quadruples
  ;   CFormat = triples
  ),

  findall(S, rdf_db:rdf(S, _, _, G), Ss1),
  sort(Ss1, Ss2),
  maplist(write_subject(State, BPrefix, G, CFormat), Ss2),

  ctriples_write_end(State, Opts).



%! write_subject(
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   ?Graph:atom,
%!   +CFormat:or([quadruples,triples]),
%!   +Subject:or([bnode,iri])
%! ) is det.
% Writes all triples that occur with the given subject term,
% possibly restricted to a given graph.
%
% Collects a sorted list of predicate-object pairs.
% Then processes each pairs -- and thus each triple -- separately.

% Format: C-Quads
write_subject(State, BPrefix, G, quadruples, S):-
  findall(P-O-G, rdf_db:rdf(S, P, O, G:_), POGTriples1),
  sort(POGTriples1, POGTriples2),
  forall(
    member(P-O-G, POGTriples2),
    (
      inc_number_of_triples(State),
      write_quadruple(S, P, O, G, BPrefix)
    )
  ).
% Format: C-Triples
write_subject(State, BPrefix, G, triples, S):-
  findall(P-O, rdf_db:rdf(S, P, O, G:_), POPairs1),
  sort(POPairs1, POPairs2),
  forall(
    member(P-O, POPairs2),
    (
      inc_number_of_triples(State),
      write_triple(S, P, O, BPrefix)
    )
  ).

