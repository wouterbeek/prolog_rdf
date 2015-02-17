:- module(
  ctriples_write_generics,
  [
    ctriples_write_begin/3, % +State:compound
                            % +BNodePrefix:iri
                            % +Options:list(nvpair)
    ctriples_write_end/2, % +State:compound
                          % +Options:list(nvpair)
    inc_number_of_triples/1, % +State:compound
    write_quadruple/5, % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % +Object:rdf_term
                       % +Graph:atom
                       % +BNodePrefix:atom
    write_literal/1, % +Literal:compound
    write_triple/4 % +Subject:or([bnode,iri])
                   % +Predicate:iri
                   % +Object:rdf_term
                   % +BNodePrefix:atom
  ]
).

/** <module> C-Triples write: generics

Generic predicates for writing C-Triples.

@author Wouter Beek
@author Jan Wielemaker
@author Laurens Rietveld
@compat http://www.w3.org/TR/2014/REC-n-triples-20140225/
@version 2014/03-2014/06, 2014/08-2014/09, 2015/01
*/

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/turtle)). % Private predicates.

:- use_module(plRdf(syntax/rdf_bnode_write)).

:- predicate_options(ctriples_write_begin/3, 3, [
     bnode_base(+atom)
   ]).
:- predicate_options(ctriples_write_begin/2, 2, [
     number_of_triples(-nonneg)
   ]).





%! ctriples_write_begin(
%!   -State:compound,
%!   -BNodePrefix:iri,
%!   +Options:list(nvpair)
%! ) is det.

ctriples_write_begin(State, BNodePrefix, Options):-
  reset_bnode_prefix_admin,

  % Keep track of the number of triples written.
  State = state(0),

  % Process the option for replacing blank nodes with IRIs,
  % establishing the prefix for each blank node.
  (   option(bnode_base(Scheme-Authority-Hash), Options)
  ->  rdf_bnode_prefix(Scheme, Authority, Hash, BNodePrefix)
  ;   rdf_bnode_prefix(BNodePrefix)
  ).



%! ctriples_write_end(+State:compound, +Options:list(nvpair)) is det.

ctriples_write_end(State, Options):-
  % Statistics option: number of triples written.
  (   option(number_of_triples(TripleCount), Options)
  ->  arg(1, State, TripleCount)
  ;   true
  ).



%! inc_number_of_triples(+State:compound) is det.

inc_number_of_triples(State):-
  arg(1, State, C0),
  C1 is C0 + 1,
  nb_setarg(1, State, C1).



%! write_quadruple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   +Graph:atom,
%!   +BNodePrefix:atom
%! ) is det.

write_quadruple(S, P, O, G, BNodePrefix):-
  write_subject(S, BNodePrefix),
  put_char(' '),
  % Predicate terms are IRIs.
  turtle:turtle_write_uri(current_output, P),
  put_char(' '),
  write_object(O, BNodePrefix),
  put_char(' '),
  % Named graphs are IRIs.
  (   G == user
  ->  true
  ;   turtle:turtle_write_uri(current_output, G)
  ),
  put_char(' '),
  put_char('.'),
  put_code(10).



%! write_triple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   +BNodePrefix:atom
%! ) is det.

write_triple(S, P, O, BNodePrefix):-
  write_subject(S, BNodePrefix),
  put_char(' '),
  % Predicate terms are IRIs.
  turtle:turtle_write_uri(current_output, P),
  put_char(' '),
  write_object(O, BNodePrefix),
  put_char(' '),
  put_char('.'),
  put_code(10).


% Object term: literal.
write_object(Literal, _):-
  write_literal(Literal), !.
% Object term: blank node
write_object(BNode, BNodePrefix):-
  rdf_is_bnode(BNode), !,
  rdf_bnode_prefix_write(BNodePrefix, BNode).
% Object term: IRI
write_object(Iri, _):-
  turtle:turtle_write_uri(current_output, Iri).

% Object term: typed literal.
write_literal(literal(type(Datatype,Value1))):- !,
  % XSD XML literal.
  (   rdf_equal(Datatype, rdf:'XMLLiteral')
  ->  with_output_to(atom(Value2), xml_write(Value1, [header(false)]))
  ;   Value2 = Value1
  ),
  % Convert numbers to atoms.
  (   number(Value2)
  ->  atom_number(Value3, Value2)
  ;   Value3 = Value2
  ),
  turtle:turtle_write_quoted_string(current_output, Value3),
  write('^^'),
  % Datatypes are IRIs.
  turtle:turtle_write_uri(current_output, Datatype).
% Object term: language-tagged string.
write_literal(literal(lang(Language,Value))):- !,
  turtle:turtle_write_quoted_string(current_output, Value),
  format(current_output, '@~w', [Language]).
% Object term: string.
write_literal(literal(Value)):- !,
  turtle:turtle_write_quoted_string(current_output, Value).


% Subject term: blank node
write_subject(BNode, BNodePrefix):-
  rdf_is_bnode(BNode), !,
  rdf_bnode_prefix_write(BNodePrefix, BNode).
% Subject term: IRI
write_subject(Iri, _):-
  turtle:turtle_write_uri(current_output, Iri).

