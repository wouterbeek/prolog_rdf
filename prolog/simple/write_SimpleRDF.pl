:- module(
  write_SimpleRDF,
  [
    write_simple_begin/4, % -BNodePrefix:atom
                          % -TripleCounter:compound
                          % -QuadrupleCounter:compound
                          % +Options:list(compound)
    write_simple_end/3, % +TripleCounter:compound
                        % +QuadrupleCounter:compound
                        % +Options:list(compound)
    write_simple_graph/2, % ?Graph:atom
                          % +Options:list(compound)
    write_simple_quadruple/6, % +BNodePrefix:atom
                              % +QuadrupleCounter:compound
                              % +Subject:iri
                              % +Predicate:iri
                              % +Object:or([iri,literal])
                              % +Graph:iri
    write_simple_statement/4, % +BNodePrefix:atom
                              % +TripleCounter:compound
                              % +QuadrupleCounter:compound
                              % +Statement:compound
    write_simple_triple/5 % +BNodePrefix:atom
                          % +TripleCounter:compound
                          % +Subject:iri
                          % +Predicate:iri
                          % +Object:or([iri,literal])
  ]
).

/** <module> Write SimpleRDF

Serialize RDF in the simplest standards-compliant format
that we can come up with.

In SimpleRDF only short Turtle strings
(delimited by a single double quote) occur.
Linefeeds and carriage returns are escaped.
This means that we can guarantee that the number of triples
is the same as the number of lines in the generated file.

Also the number of spaces between the RDF terms is exactly one
and lines end in one space, one dot, and a newline character.

Every literal that is not a language-tagged string receives a datatype IRI,
assuming `xsd:string` in case no datatype IRI is given.

@author Wouter Beek
@author Jan Wielemaker
@author Laurens Rietveld
@version 2015/08, 2015/10-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/thread_counter)).
:- use_module(library(rdf/rdf_bnode_name)). % Private
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/turtle)). % Private
:- use_module(library(typecheck)).
:- use_module(library(uri)).

:- rdf_meta(write_simple_quadruple(+,+,r,r,o,+)).
:- rdf_meta(write_simple_triple(+,+,r,r,o)).

:- predicate_options(write_simple_begin/4, 4, [
     base_iri(+atom)
   ]).
:- predicate_options(write_simple_end/3, 3, [
     quadruples(-nonneg),
     statements(-nonneg),
     triples(-nonneg)
   ]).
:- predicate_options(write_simple_graph/2, 2, [
     format(+oneof([quadruples,triples])),
     pass_to(write_simple_begin/4, 4),
     pass_to(write_simple_end/3, 3)
   ]).




%! write_simple_begin(
%!   -BNodePrefix:atom,
%!   -TripleCounter:compound,
%!   -QuadrupleCounter:compound,
%!   +Options:list(compound)
%! ) is det.

write_simple_begin(BNodePrefix, triples, quadruples, Opts):-
  reset_bnode_names,

  create_thread_counter(triples),
  create_thread_counter(quadruples),

  % Process the option for replacing blank nodes with IRIs,
  % establishing the prefix for each blank node.
  (   option(base_iri(BaseIri), Opts)
  ->  uri_components(BaseIri, uri_components(Scheme,Auth,Path0,_,_)),
      atom_ending_in(Path0, '#', Suffix),
      atomic_list_concat(['','.well-known',genid,Suffix], /, Path),
      uri_components(BNodePrefix, uri_components(Scheme,Auth,Path,_,_))
  ;   BNodePrefix = '_:'
  ).



%! write_simple_bnode(+BNodePrefix:uri, +BNode:atom) is det.

write_simple_bnode(BNodePrefix, BNode):-
  rdf_bnode_name:rdf_bnode_name0(BNodePrefix, BNode, BNodeName),
  turtle:turtle_write_uri(current_output, BNodeName).



%! write_simple_end(
%!   +TripleCounter:compound,
%!   +QuadrupleCounter:compound,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * quadruples(-nonneg)
%   * statements(-nonneg)
%   * triples(-nonneg)

write_simple_end(CT, CQ, Opts):-
  delete_thread_counter(CT, NT),
  option(triples(NT), Opts, _),

  delete_thread_counter(CQ, NQ),
  option(quadruples(NQ), Opts, _),

  NS is NT + NQ,
  option(statements(NS), Opts, _),

  reset_bnode_names.



%! write_simple_graph(?Graph:atom, +Options:list(compound)) is det.
% The following options are supported:
%   * format(?oneof([quadruples,triples]))
%   * quadruples(-nonneg)
%   * statements(-nonneg)
%   * triples(-nonneg)

write_simple_graph(G, Opts):-
  write_simple_begin(BNodePrefix, CT, CQ, Opts),

  % Decide whether triples or quadruples are written.
  option(format(Format), Opts, _),
  (   nonvar(Format)
  ->  must_be(oneof([quadruples,triples]), Format)
  ;   rdf_graph(G),
      G \== default,
      rdf(_, _, _, G)
  ->  must_be(iri, G),
      Format = quadruples
  ;   Format = triples
  ),

  aggregate_all(set(S), rdf(S, _, _, G), Ss),
  maplist(write_simple_subject(BNodePrefix, CT, CQ, G, Format), Ss),

  write_simple_end(CT, CQ, Opts).



% Object term: typed literal.
write_simple_literal(literal(type(D,Lex))):- !,
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  % Datatypes are IRIs.
  turtle:turtle_write_uri(current_output, D).
% Object term: language-tagged string.
write_simple_literal(literal(lang(LTag,Lex))):- !,
  turtle:turtle_write_quoted_string(current_output, Lex),
  format(current_output, '@~w', [LTag]).
% Object term: string.
write_simple_literal(literal(Lex)):- !,
  turtle:turtle_write_quoted_string(current_output, Lex).



% Object term: literal.
write_simple_object(Lit, _):-
  write_simple_literal(Lit), !.
% Object term: blank node
write_simple_object(BNode, BNodePrefix):-
  rdf_is_bnode(BNode), !,
  write_simple_bnode(BNodePrefix, BNode).
% Object term: IRI
write_simple_object(Iri, _):-
  turtle:turtle_write_uri(current_output, Iri).



%! write_simple_quadruple(
%!   +BNodePrefix:atom,
%!   +Counter:compound,
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   +Graph:atom
%! ) is det.

write_simple_quadruple(BNodePrefix, CQ, S, P, O, G):-
  write_simple_subject(S, BNodePrefix),
  put_char(' '),
  % Predicate terms are IRIs.
  turtle:turtle_write_uri(current_output, P),
  put_char(' '),
  write_simple_object(O, BNodePrefix),
  put_char(' '),
  % Named graphs are IRIs.
  (   G == default
  ->  true
  ;   turtle:turtle_write_uri(current_output, G)
  ),
  put_char(' '),
  put_char(.),
  put_code(10),
  increment_thread_counter(CQ).



%! write_simple_statement(
%!   +BNodePrefix:atom,
%!   +TripleCounter:compound,
%!   +QuadrupleCounter:compound,
%!   +Statement:compound
%! ) is det.

write_simple_statement(BNodePrefix, CT, _, rdf(S,P,O)):- !,
  write_simple_triple(BNodePrefix, CT, S, P, O).
write_simple_statement(BNodePrefix, _, CQ, rdf(S,P,O,G)):-
  write_simple_quadruple(BNodePrefix, CQ, S, P, O, G).



% Subject term: blank node
write_simple_subject(BNode, BNodePrefix):-
  rdf_is_bnode(BNode), !,
  write_simple_bnode(BNodePrefix, BNode).
% Subject term: IRI
write_simple_subject(Iri, _):-
  turtle:turtle_write_uri(current_output, Iri).



%! write_simple_subject(
%!   +BNodePrefix:atom,
%!   +TripleCounter:compound,
%!   +QuadrupleCounter:compound,
%!   ?Graph:atom,
%!   +Format:oneof([quadruple,triple]),
%!   +Subject:or([bnode,iri])
%! ) is det.
% Writes all triples that occur with the given subject term,
% possibly restricted to a given graph.
%
% Collects a sorted list of predicate-object pairs.
% Then processes each pairs -- and thus each triple -- separately.

% Format: Quadruples.
write_simple_subject(BNodePrefix, _, CQ, G, quadruple, S):-
  aggregate_all(set(P-O-G), rdf(S, P, O, G), POGs),
  forall(member(P-O-G, POGs), write_simple_quadruple(BNodePrefix, CQ, S, P, O, G)).
% Format: Triples.
write_simple_subject(BNodePrefix, CT, _, G, triple, S):-
  aggregate_all(set(P-O), rdf(S, P, O, G), POs),
  forall(member(P-O, POs), write_simple_triple(BNodePrefix, CT, S, P, O)).



%! write_simple_triple(
%!   +BNodePrefix:atom,
%!   +Counter:compound,
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term
%! ) is det.

write_simple_triple(BNodePrefix, Counter, S, P, O):-
  write_simple_subject(S, BNodePrefix),
  put_char(' '),
  % Predicate terms are IRIs.
  turtle:turtle_write_uri(current_output, P),
  put_char(' '),
  write_simple_object(O, BNodePrefix),
  put_char(' '),
  put_char(.),
  put_code(10),
  increment_thread_counter(Counter).
