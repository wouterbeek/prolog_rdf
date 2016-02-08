:- module(
  write_SimpleRDF,
  [
    write_simple_begin/4,     % -BPrefix, -TripCounter:compound, -QuadCounter:compound, +Opts
    write_simple_end/3,       % +TripCounter:compound, +QuadCounter:compound, +Opts
    write_simple_graph/2,     % ?G, +Opts
    write_simple_quadruple/4, % +S, +P, +O, +G
    write_simple_quadruple/5, % +BPrefix, +S, +P, +O, +G
    write_simple_quadruple/6, % +BPrefix, +QuadCounter:compound, +S, +P, +O, +G
    write_simple_statement/4, % +BPrefix, +TripCounter:compound, +QuadCounter:compound, +Statement
    write_simple_triple/3,    % +S, +P, +O
    write_simple_triple/4,    % +BPrefix, +S, +P, +O
    write_simple_triple/5     % +BPrefix, +TripCounter:compound, +S, +P, +O
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
@version 2015/08, 2015/10-2016/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/thread_counter)).
:- use_module(library(rdf/rdf_bnode_name)). % Private
:- use_module(library(rdf11/rdf11)).
:- use_module(library(semweb/turtle)). % Private
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(iri/rfc3987_gen)).

:- rdf_meta
   write_simple_quadruple(r,r,o,+),
   write_simple_quadruple(+,r,r,o,+),
   write_simple_quadruple(+,+,r,r,o,+),
   write_simple_triple(r,r,o),
   write_simple_triple(+,r,r,o),
   write_simple_triple(+,+,r,r,o).

:- predicate_options(write_simple_begin/4, 4, [
     base_iri(+atom)
   ]).
:- predicate_options(write_simple_end/3, 3, [
     quadruples(-nonneg),
     statements(-nonneg),
     triples(-nonneg)
   ]).
:- predicate_options(write_simple_graph/2, 2, [
     rdf_format(+oneof([quadruples,triples])),
     pass_to(write_simple_begin/4, 4),
     pass_to(write_simple_end/3, 3)
   ]).




%! write_simple_begin(
%!   -BPrefix:atom,
%!   -TripleCounter:compound,
%!   -QuadrupleCounter:compound,
%!   +Options:list(compound)
%! ) is det.

write_simple_begin(BPrefix, triples, quadruples, Opts) :-
  reset_bnode_names,

  create_thread_counter(triples),
  create_thread_counter(quadruples),

  % Process the option for replacing blank nodes with IRIs,
  % establishing the prefix for each blank node.
  (   option(base_iri(BaseIri), Opts)
  ->  uri_components(BaseIri, uri_components(Scheme,Auth,Path0,_,_)),
      atom_ending_in(Path0, '#', Suffix),
      atomic_list_concat(['','.well-known',genid,Suffix], /, Path),
      uri_components(BPrefix, uri_components(Scheme,Auth,Path,_,_))
  ;   BPrefix = '_:'
  ).



%! write_simple_bnode(+BPrefix:uri, +B:atom) is det.

write_simple_bnode(BPrefix, B) :-
  rdf_bnode_name:rdf_bnode_name0(BPrefix, B, BName),
  turtle:turtle_write_uri(current_output, BName).



%! write_simple_end(
%!   +TripleCounter:compound,
%!   +QuadrupleCounter:compound,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * quadruples(-nonneg)
%   * statements(-nonneg)
%   * triples(-nonneg)

write_simple_end(CT, CQ, Opts) :-
  delete_thread_counter(CT, NT),
  option(triples(NT), Opts, _),

  delete_thread_counter(CQ, NQ),
  option(quadruples(NQ), Opts, _),

  NS is NT + NQ,
  option(statements(NS), Opts, _),

  reset_bnode_names.



%! write_simple_graph(?Graph:atom, +Options:list(compound)) is det.
% The following options are supported:
%   * quadruples(-nonneg)
%   * rdf_format(?oneof([nquads,ntriples]))
%   * statements(-nonneg)
%   * triples(-nonneg)

write_simple_graph(G, Opts) :-
  write_simple_begin(BPrefix, CT, CQ, Opts),
  option(rdf_format(F), Opts, _),
  (   ground(F)
  ->  must_be(oneof([nquads,ntriples]), Format)
  ;   rdf_graph(G),
      G \== default,
      rdf(_, _, _, G)
  ->  must_be(iri, G),
      F = nquads
  ;   F = ntriples
  ),
  aggregate_all(set(S), rdf(S, _, _, G), Ss),
  maplist(write_simple_subject(BPrefix, CT, CQ, G, F), Ss),
  write_simple_end(CT, CQ, Opts).



%! write_simple_graph_term(+G) is det.

write_simple_graph_term(default) :- !.
write_simple_graph_term(G)       :- write_simple_iri(G).



%! write_simple_iri(+Iri) is det.

write_simple_iri(Iri) :-
  turtle:turtle_write_uri(current_output, Iri).



%! write_simple_literal(+Lit) is det.

% Typed literal: current representation.
write_simple_literal(V^^D) :- !,
  rdf11:in_type(D, V, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  turtle:turtle_write_uri(current_output, D).
% Typed literal: legacy representation.
write_simple_literal(literal(type(D,Lex))) :- !,
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  turtle:turtle_write_uri(current_output, D).
% Language-tagged string: current representation.
write_simple_literal(Lex0@LTag) :- !,
  atom_string(Lex, Lex0),
  turtle:turtle_write_quoted_string(current_output, Lex),
  format(current_output, '@~w', [LTag]).
% Language-tagged string: legacy representation.
write_simple_literal(literal(lang(LTag,Lex))) :- !,
  turtle:turtle_write_quoted_string(current_output, Lex),
  format(current_output, '@~w', [LTag]).
% Implicit XSD string literal.
write_simple_literal(literal(Lex)) :-
  rdf_equal(xsd:string, D),
  write_simple_literal(literal(type(D,Lex))).



%! write_simple_object(+O, +BPrefix) is det.

% Object term: literal.
write_simple_object(Lit, _) :-
  write_simple_literal(Lit), !.
% Object term: blank node
write_simple_object(B, BPrefix) :-
  rdf_is_bnode(B), !,
  write_simple_bnode(BPrefix, B).
% Object term: IRI
write_simple_object(Iri, _) :-
  write_simple_iri(Iri).



%! write_simple_predicate(+P) is det.

write_simple_predicate(P) :-
  write_simple_iri(P).



%! write_simple_quadruple(+S, +P, +O, +G) is det.
%! write_simple_quadruple(+Counter:compound, +S, +P, +O, +G) is det.
%! write_simple_quadruple(+BPrefix, +Counter:compound, +S, +P, +O, +G) is det.

write_simple_quadruple(S, P, O, G) :-
  write_simple_quadruple('_:', S, P, O, G).

write_simple_quadruple(BPrefix, S, P, O, G) :-
  write_simple_subject(S, BPrefix),
  put_char(' '),
  write_simple_predicate(P),
  put_char(' '),
  write_simple_object(O, BPrefix),
  put_char(' '),
  write_simple_graph_term(G),
  put_char(' '),
  put_char(.),
  put_code(10).

write_simple_quadruple(BPrefix, C, S, P, O, G) :-
  write_simple_quadruple(BPrefix, S, P, O, G),
  increment_thread_counter(C).



%! write_simple_statement(
%!   +BPrefix:atom,
%!   +TripleCounter:compound,
%!   +QuadrupleCounter:compound,
%!   +Statement:compound
%! ) is det.

write_simple_statement(BPrefix, CT, _, rdf(S,P,O)) :- !,
  write_simple_triple(BPrefix, CT, S, P, O).
write_simple_statement(BPrefix, _, CQ, rdf(S,P,O,G)) :-
  write_simple_quadruple(BPrefix, CQ, S, P, O, G).



%! write_simple_subject(+S, +BPrefix) is det.

% Subject term: blank node
write_simple_subject(B, BPrefix) :-
  rdf_is_bnode(B), !,
  write_simple_bnode(BPrefix, B).
% Subject term: IRI
write_simple_subject(Iri, _) :-
  write_simple_iri(Iri).



%! write_simple_subject(
%!   +BPrefix,
%!   +TripCounter:compound,
%!   +QuadCounter:compound,
%!   ?Graph,
%!   +Format:oneof([nquads,ntriples]),
%!   +S
%! ) is det.
% Writes all triples that occur with the given subject term,
% possibly restricted to a given graph.
%
% Collects a sorted list of predicate-object pairs.
% Then processes each pairs -- and thus each triple -- separately.

% Format: Quadruples.
write_simple_subject(BPrefix, _, CQ, G, quadruple, S) :-
  aggregate_all(set(P-O-G), rdf(S, P, O, G), POGs),
  forall(member(P-O-G, POGs), write_simple_quadruple(BPrefix, CQ, S, P, O, G)).
% Format: Triples.
write_simple_subject(BPrefix, CT, _, G, triple, S) :-
  aggregate_all(set(P-O), rdf(S, P, O, G), POs),
  forall(member(P-O, POs), write_simple_triple(BPrefix, CT, S, P, O)).



%! write_simple_triple(+S, +P, +O) is det.
%! write_simple_triple(+BPrefix:atom, +S, +P, +O) is det.
%! write_simple_triple(+BPrefix:atom, +Counter:compound, +S, +P, +O) is det.

write_simple_triple(S, P, O) :-
  write_simple_triple('_:', S, P, O).

write_simple_triple(BPrefix, S, P, O) :-
  write_simple_subject(S, BPrefix),
  put_char(' '),
  write_simple_predicate(P),
  put_char(' '),
  write_simple_object(O, BPrefix),
  put_char(' '),
  put_char(.),
  put_code(10).

write_simple_triple(BPrefix, Counter, S, P, O) :-
  write_simple_triple(BPrefix, S, P, O),
  increment_thread_counter(Counter).
