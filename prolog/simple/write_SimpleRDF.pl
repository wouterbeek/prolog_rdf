:- module(
  write_SimpleRDF,
  [
    write_simple_begin/4,  % -BPrefix, -TripleCounter:compound, -QuadCounter:compound, +Opts
    write_simple_end/3,    % +TripleCounter:compound, +QuadCounter:compound, +Opts
    write_simple_graph/3,  % ?Pos, ?G, +Opts
    write_simple_quad/5,   % ?Pos, +S, +P, +O, +G
    write_simple_quad/6,   % +BPrefix, ?Pos, +S, +P, +O, +G
    write_simple_quad/7,   % +BPrefix, +QuadCounter:compound, ?Pos, +S, +P, +O, +G
    write_simple_tuple/5,  % +BPrefix, +TripleCounter:compound, +QuadCounter:compound, ?Pos, +Tuple
    write_simple_triple/3, % +S, +P, +O
    write_simple_triple/4, % ?Pos, +S, +P, +O
    write_simple_triple/5, % +BPrefix, ?Pos, +S, +P, +O
    write_simple_triple/6  % +BPrefix, ?Pos, +TripleCounter:compound, +S, +P, +O
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
@version 2015/08, 2015/10-2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/thread_counter)).
:- use_module(library(rdf/rdf_bnode_name)). % Private
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle)). % Private
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(iri/rfc3987_gen)).

:- rdf_meta
   write_simple_quad(?, r, r, o, +),
   write_simple_quad(+, ?, r, r, o, +),
   write_simple_quad(+, +, ?, r, r, o, +),
   write_simple_triple(r, r, o),
   write_simple_triple(?, r, r, o),
   write_simple_triple(+, ?, r, r, o),
   write_simple_triple(+, +, ?, r, r, o).

:- predicate_options(write_simple_begin/4, 4, [
     base_iri(+atom)
   ]).
:- predicate_options(write_simple_end/3, 3, [
     quads(-nonneg),
     triples(-nonneg),
     tuples(-nonneg)
   ]).
:- predicate_options(write_simple_graph/3, 3, [
     rdf_format(+oneof([quads,triples])),
     pass_to(write_simple_begin/4, 4),
     pass_to(write_simple_end/3, 3)
   ]).




%! write_simple_begin(
%!   -BPrefix,
%!   -TripleleCounter:compound,
%!   -QuadCounter:compound,
%!   +Opts
%! ) is det.

write_simple_begin(BPrefix, triples, quads, Opts) :-
  reset_bnode_names,

  create_thread_counter(triples),
  create_thread_counter(quads),

  % Process the option for replacing blank nodes with IRIs,
  % establishing the prefix for each blank node.
  (   option(base_iri(BaseIri), Opts)
  ->  uri_components(BaseIri, uri_components(Scheme,Auth,Path0,_,_)),
      atom_ending_in(Path0, '#', Suffix),
      atomic_list_concat(['','.well-known',genid,Suffix], /, Path),
      uri_components(BPrefix, uri_components(Scheme,Auth,Path,_,_))
  ;   BPrefix = '_:'
  ).



%! write_simple_bnode(+BPrefix, +B) is det.

write_simple_bnode(BPrefix, B) :-
  rdf_bnode_name:rdf_bnode_name0(BPrefix, B, BName),
  write(BName).



%! write_simple_end(+TripleCounter:compound, +QuadCounter:compound, +Opts) is det.
% The following options are supported:
%   - quads(-nonneg)
%   - triples(-nonneg)
%   - tuples(-nonneg)

write_simple_end(CTriples, CQuads, Opts) :-
  delete_thread_counter(CTriples, NTriples),
  option(triples(NTriples), Opts, _),

  delete_thread_counter(CQuads, NQuads),
  option(quads(NQuads), Opts, _),

  NTuples is NTriples + NQuads,
  option(tuples(NTuples), Opts, _),

  reset_bnode_names.



%! write_simple_graph(?Pos, ?G, +Opts) is det.
% The following options are supported:
%   - quads(-nonneg)
%   - rdf_format(?oneof([nquads,ntriples]))
%   - triples(-nonneg)
%   - tuples(-nonneg)

write_simple_graph(Pos, G, Opts) :-
  write_simple_begin(BPrefix, CT, CQ, Opts),
  option(rdf_format(F), Opts, _),
  (   ground(F)
  ->  must_be(oneof([nquads,ntriples]), F)
  ;   rdf_graph(G),
      G \== default,
      rdf(_, _, _, G)
  ->  must_be(iri, G),
      F = nquads
  ;   F = ntriples
  ),
  aggregate_all(set(S), rdf(S, _, _, G), Ss),
  maplist(write_simple_subject(BPrefix, CT, CQ, Pos, G, F), Ss),
  write_simple_end(CT, CQ, Opts).



%! write_simple_graph_term(+G) is det.

write_simple_graph_term(default) :- !.
write_simple_graph_term(G)       :- write_simple_iri(G).



%! write_simple_iri(+Iri) is det.

write_simple_iri(Iri) :-
  turtle:turtle_write_uri(current_output, Iri).



%! write_simple_literal(?Pos, +Lit) is det.

write_simple_literal(_, V^^D) :- !,
  rdf_literal_lexical_form(V^^D, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  turtle:turtle_write_uri(current_output, D).
write_simple_literal(_, V@LTag) :- !,
  rdf_literal_lexical_form(V@LTag, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  format(current_output, '@~w', [LTag]).
% Literal legacy representations.
write_simple_literal(Pos, Lit0) :-
  rdf_legacy_literal_components(Lit0, D, Lex0, LTag0),
  rdf11:pre_object(Lit, Lit0),
  rdf_literal_components(Lit, D, Lex, LTag),
  (   Lex == Lex0
  ->  true
  ;   increment_thread_counter(rdf_warning),
      writeln(msg, error(non_canonical_lex(D,Lex),Pos))
  ),
  (   LTag == LTag0
  ->  true
  ;   increment_thread_counter(rdf_warning),
      writeln(msg, error(non_canonical_ltag(LTag),Pos))
  ),
  write_simple_literal(Lit).



%! write_simple_object(+BPrefix, ?Pos, +O) is det.

% Object term: literal.
write_simple_object(_, Pos, Lit) :-
  write_simple_literal(Pos, Lit), !.
% Object term: blank node
write_simple_object(BPrefix, _, B) :-
  rdf_is_bnode(B), !,
  write_simple_bnode(BPrefix, B).
% Object term: IRI
write_simple_object(_, _, Iri) :-
  write_simple_iri(Iri).



%! write_simple_predicate(+P) is det.

write_simple_predicate(P) :-
  write_simple_iri(P).



%! write_simple_quad(?Pos, +S, +P, +O, +G) is det.
%! write_simple_quad(+BPrefix, ?Pos, +S, +P, +O, +G) is det.
%! write_simple_quad(+BPrefix, +Counter:compound, ?Pos, +S, +P, +O, +G) is det.

write_simple_quad(Pos, S, P, O, G) :-
  write_simple_quad('_:', Pos, S, P, O, G).

write_simple_quad(BPrefix, Pos, S, P, O, G) :-
  write_simple_subject(BPrefix, S),
  put_char(' '),
  write_simple_predicate(P),
  put_char(' '),
  write_simple_object(BPrefix, Pos, O),
  put_char(' '),
  write_simple_graph_term(G),
  put_char(' '),
  put_char(.),
  put_code(10).

write_simple_quad(BPrefix, C, Pos, S, P, O, G) :-
  write_simple_quad(BPrefix, Pos, S, P, O, G),
  increment_thread_counter(C).



%! write_simple_tuple(
%!   +BPrefix,
%!   +TripleleCounter:compound,
%!   +QuadCounter:compound,
%!   ?Pos,
%!   +Tuple
%! ) is det.

write_simple_tuple(BPrefix, CT, _, Pos, rdf(S,P,O)) :- !,
  write_simple_triple(BPrefix, CT, Pos, S, P, O).
write_simple_tuple(BPrefix, _, CQ, Pos, rdf(S,P,O,G)) :-
  write_simple_quad(BPrefix, CQ, Pos, S, P, O, G).



%! write_simple_subject(+BPrefix, +S) is det.

% Subject term: blank node
write_simple_subject(BPrefix, B) :-
  rdf_is_bnode(B), !,
  write_simple_bnode(BPrefix, B).
% Subject term: IRI
write_simple_subject(_, Iri) :-
  write_simple_iri(Iri).



%! write_simple_subject(
%!   +BPrefix,
%!   +TripleCounter:compound,
%!   +QuadCounter:compound,
%!   ?Pos,
%!   ?G,
%!   +Format:oneof([nquads,ntriples]),
%!   +S
%! ) is det.
% Writes all triples that occur with the given subject term,
% possibly restricted to a given graph.
%
% Collects a sorted list of predicate-object pairs.
% Then processes each pairs -- and thus each triple -- separately.

write_simple_subject(BPrefix, _, CQ, Pos, G, quad, S) :-
  aggregate_all(set(P-O-G), rdf(S, P, O, G), POGs),
  forall(member(P-O-G, POGs), write_simple_quad(BPrefix, CQ, Pos, S, P, O, G)).
write_simple_subject(BPrefix, CT, _, Pos, G, triple, S) :-
  aggregate_all(set(P-O), rdf(S, P, O, G), POs),
  forall(member(P-O, POs), write_simple_triple(BPrefix, CT, Pos, S, P, O)).



%! write_simple_triple(+S, +P, +O) is det.
%! write_simple_triple(?Pos, +S, +P, +O) is det.
%! write_simple_triple(+BPrefix:atom, ?Pos, +S, +P, +O) is det.
%! write_simple_triple(+BPrefix:atom, +Counter:compound, ?Pos, +S, +P, +O) is det.

write_simple_triple(S, P, O) :-
  write_simple_triple(_, S, P, O).

write_simple_triple(Pos, S, P, O) :-
  write_simple_triple('_:', Pos, S, P, O).

write_simple_triple(BPrefix, Pos, S, P, O) :-
  write_simple_subject(BPrefix, S),
  put_char(' '),
  write_simple_predicate(P),
  put_char(' '),
  write_simple_object(BPrefix, Pos, O),
  put_char(' '),
  put_char(.),
  put_code(10).

write_simple_triple(BPrefix, Counter, Pos, S, P, O) :-
  write_simple_triple(BPrefix, Pos, S, P, O),
  increment_thread_counter(Counter).
