:- module(
  ctriples_write_generics,
  [
    ctriples_write_begin/3, % +State:compound
                            % +BPrefix:iri
                            % +Options:list(compound)
    ctriples_write_end/2, % +State:compound
                          % +Options:list(compound)
    inc_number_of_triples/1, % +State:compound
    write_quadruple/5, % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % +Object:rdf_term
                       % +Graph:atom
                       % +BPrefix:atom
    write_literal/1, % +Literal:compound
    write_triple/4 % +Subject:or([bnode,iri])
                   % +Predicate:iri
                   % +Object:rdf_term
                   % +BPrefix:atom
  ]
).

/** <module> C-Triples write: generics

Generic predicates for writing C-Triples.

@author Wouter Beek
@author Jan Wielemaker
@author Laurens Rietveld
@compat http://www.w3.org/TR/2014/REC-n-triples-20140225/
@version 2015/08
*/

:- use_module(library(option)).
:- use_module(library(rdf/rdf_bnode_name)). % Private predicates.
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)). % Private predicates.

:- predicate_options(ctriples_write_begin/3, 3, [
     bnode_base(+atom)
   ]).
:- predicate_options(ctriples_write_begin/3, 3, [
     number_of_triples(-nonneg)
   ]).

:- thread_local(number_of_triples/1).





%! ctriples_write_begin(
%!   -State:compound,
%!   -BPrefix:iri,
%!   +Options:list(compound)
%! ) is det.

ctriples_write_begin(_, BPrefix, Opts):-
  reset_bnode_names,

  % Keep track of the number of triples written.
  reset_number_of_triples(_),

  % Process the option for replacing blank nodes with IRIs,
  % establishing the prefix for each blank node.
  (   option(bnode_base(Scheme-Auth-Name0), Opts)
  ->  rdf_well_known_bnode_prefix(Scheme, Auth, Name0, BPrefix)
  ;   throw(missing_bnode_base)
  ).



%! ctriples_write_end(+State:compound, +Options:list(compound)) is det.

ctriples_write_end(_, Opts):-
  % Statistics option: number of triples written.
  (   option(number_of_triples(N), Opts)
  ->  reset_number_of_triples(N)
  ;   true
  ).



%! inc_number_of_triples(+State:compound) is det.

inc_number_of_triples(State):-
  var(State), !.
inc_number_of_triples(_State):-
  retract(number_of_triples(N0)),
  N is N0 + 1,
  assert(number_of_triples(N)).



%! rdf_bnode_write(+Prefix:uri, +BNode:atom) is det.

rdf_bnode_write(BPrefix, BNode):-
  rdf_bnode_name:rdf_bnode_name0(BPrefix, BNode, BName),
  turtle:turtle_write_uri(current_output, BName).



%! rdf_well_known_bnode_prefix(
%!   +Scheme:atom,
%!   +Authority:atom,
%!   +Name:atom,
%!   -BNodePrefix:atom
%! ) is det.

rdf_well_known_bnode_prefix(Scheme, Auth, Name0, BNodePrefix):-
  atomic_concat(Name0, '#', Suffix),
  atomic_list_concat(['','.well-known',genid,Suffix], /, Path),
  uri_components(BNodePrefix, uri_components(Scheme,Auth,Path,_,_)).



%! reset_number_of_triples(-NumberOfTriples:nonneg) is det.

reset_number_of_triples(N):-
  ignore(retract(number_of_triples(N))),
  assert(number_of_triples(0)).



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



% Object term: literal.
write_object(Literal, _):-
  write_literal(Literal), !.
% Object term: blank node
write_object(BNode, BPrefix):-
  rdf_is_bnode(BNode), !,
  rdf_bnode_write(BPrefix, BNode).
% Object term: IRI
write_object(Iri, _):-
  turtle:turtle_write_uri(current_output, Iri).



%! write_quadruple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   +Graph:atom,
%!   +BPrefix:atom
%! ) is det.

write_quadruple(S, P, O, G, BPrefix):-
  write_subject(S, BPrefix),
  put_char(' '),
  % Predicate terms are IRIs.
  turtle:turtle_write_uri(current_output, P),
  put_char(' '),
  write_object(O, BPrefix),
  put_char(' '),
  % Named graphs are IRIs.
  (   G == user
  ->  true
  ;   turtle:turtle_write_uri(current_output, G)
  ),
  put_char(' '),
  put_char(.),
  put_code(10).



% Subject term: blank node
write_subject(BNode, BPrefix):-
  rdf_is_bnode(BNode), !,
  rdf_bnode_write(BPrefix, BNode).
% Subject term: IRI
write_subject(Iri, _):-
  turtle:turtle_write_uri(current_output, Iri).



%! write_triple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   +BPrefix:atom
%! ) is det.

write_triple(S, P, O, BPrefix):-
  write_subject(S, BPrefix),
  put_char(' '),
  % Predicate terms are IRIs.
  turtle:turtle_write_uri(current_output, P),
  put_char(' '),
  write_object(O, BPrefix),
  put_char(' '),
  put_char(.),
  put_code(10).
