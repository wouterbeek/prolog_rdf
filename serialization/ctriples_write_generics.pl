:- module(
  ctriples_write_generics,
  [
  ]
).

/** <module> C-Triples write: generics

Generic predicates for writing C-Triples.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(option)).



%! ctriples_write_begin(
%!   -State:compound,
%!   -BNodePrefix:iri,
%!   +Options:list(nvpair)
%! ) is det.

ctriples_write_begin(State, BNodePrefix, Options):-
  reset_bnode_admin,
  
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


%! inc_number_of_triples(+State:compound):-

inc_number_of_triples(State):-
  arg(1, State, C0),
  C1 is C0 + 1,
  nb_setarg(1, State, C1).


%! rdf_bnode_prefix(-BNodePrefix:atom) is semidet.

rdf_bnode_prefix('_:').


%! write_quad(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   +Graph:atom,
%!   +BNodePrefix:atom
%! ) is det.

write_quad(S, P, O, G, BNodePrefix):-
  write_subject(S, BNodePrefix),
  put_char(' '),
  % Predicate terms are IRIs.
  turtle:turtle_write_uri(current_output, P),
  put_char(' '),
  write_object(O, BNodePrefix),
  put_char(' '),
  % Named graphs are IRIs.
  turtle:turtle_write_uri(current_output, G),
  put_char(' '),
  put_char('.'),
  put_code(10).


%! write_triple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
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


% Object term: typed literal.
write_object(literal(type(Datatype,Value1)), _):- !,
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
write_object(literal(lang(Language,Value)), _):- !,
  turtle:turtle_write_quoted_string(current_output, Value),
  format(current_output, '@~w', [Language]).
% Object term: string.
write_object(literal(Value), _):- !,
  turtle:turtle_write_quoted_string(current_output, Value).
% Object term: blank node
write_object(BNode, BNodePrefix):-
  rdf_is_bnode(BNode), !,
  rdf_bnode_write(BNodePrefix, BNode).
% Object term: IRI
write_object(Iri, _):-
  turtle:turtle_write_uri(current_output, Iri).


% Subject term: blank node
write_subject(BNode, BNodePrefix):-
  rdf_is_bnode(BNode), !,
  rdf_bnode_write(BNodePrefix, BNode).
% Subject term: IRI
write_subject(Iri, _):-
  turtle:turtle_write_uri(current_output, Iri).

