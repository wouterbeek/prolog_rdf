:- module(
  rdf_ntriples_write,
  [
    rdf_ntriples_write/1, % +Options:list(nvpair)
    rdf_ntriples_write/2, % +Write:or([atom,stream])
                          % +Options:list(nvpair)
    rdf_write_ntriple/4 % +Subject:or([bnode,iri])
                        % +Predicate:iri
                        % +Object:or([bnode,iri,literal])
                        % +BNodePrefix:atom
  ]
).

/** <module> RDF save to N-Triples

A simple implementation for emitting RDF data in
 N-Triples serialization format, reusing some of the Turtle writer.
Intended to work with RDF data stored using SWI-Prolog's Semweb library.

In N-Triples only short Turtle strings (delimited by a single double quote)
 occur.
Linefeeds and carriage returns are escaped.
This means that we can guarantee that the number of triples
 is the same as the number of lines in the generated file.

@author Wouter Beek
@author Jan Wielemaker
@compat http://www.w3.org/TR/2014/REC-n-triples-20140225/
@tbd We would like to serialize no duplicate triples.
     Provide this at least as an option.
@version 2014/03-2014/06
*/

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)). % Private predicates.
:- use_module(library(sgml_write)).
:- use_module(library(uri)).

:- use_module(plRdf(rdf_metadata)).
:- use_module(plRdf_term(rdf_bnode)).



%! rdf_ntriples_write(+Write:or([atom,stream]), +Options:list) is det.
% Writes RDF data serialization in the N-Triples format to the given file.
%
% The following options are supported:
%   * =|bnode_base(+Iri:atom)|=
%     Replace blank nodes with an IRI, defined as per
%     RDF 1.1 spec (see link below).
%   * =|graph(+Graph:atom)|=
%     The atomic name of a currently loaded RDF graph,
%     to restrict the triples that are saved,
%     or uninstantiated, in which case
%     all currently loaded triples are saved.
%   * =|number_of_triples(-Triples:nonneg)|=
%     The number of triples that was written.
%
% @arg File The atomic name of a file.
% @arg Options A list of name-value pairs.
%
% @see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-skolemization

% Input is a stream.
rdf_ntriples_write(Write, Options):-
  is_stream(Write), !,
  with_output_to(Write, rdf_ntriples_write(Options)).
% Input is a file.
% Open a stream in `write` mode.
rdf_ntriples_write(File, Options):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, write, Write),
    with_output_to(Write, rdf_ntriples_write(Options)),
    close(Write)
  ).


rdf_ntriples_write(Options):-
  reset_bnode_admin,

  % Keep track of the number of triples written.
  State = state(0),

  % Process the option for replacing blank nodes with IRIs,
  % establishing the prefix for each blank node.
  (
    option(bnode_base(Scheme-Authority-Hash), Options)
  ->
    rdf_bnode_prefix(Scheme, Authority, Hash, BNodePrefix)
  ;
    rdf_bnode_prefix(BNodePrefix)
  ),
  
  % Whether triples are read from a specific graph or not.
  (
    option(graph(Graph), Options)
  ->
    forall(
      rdf(S, P, O, Graph:_),
      (
        inc_number_of_triples(State),
        rdf_write_ntriple(S, P, O, BNodePrefix)
      )
    )
  ;
    forall(
      % Deduplicate triples.
      rdf(S, P, O),
      (
        inc_number_of_triples(State),
        rdf_write_ntriple(S, P, O, BNodePrefix)
      )
    )
  ),

  % Statistics option: number of triples written.
  (
    option(number_of_triples(TripleCount), Options)
  ->
    arg(1, State, TripleCount)
  ;
    true
  ).


inc_number_of_triples(State) :-
  arg(1, State, C0),
  C1 is C0 + 1,
  nb_setarg(1, State, C1).


%! rdf_write_ntriple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   +BNodePrefix:atom
%! ) is det.

rdf_write_ntriple(S, P, O, BNodePrefix):-
  rdf_write_subject(S, BNodePrefix),
  put_char(' '),
  rdf_write_predicate(P),
  put_char(' '),
  rdf_write_object(O, BNodePrefix),
  put_char(' '),
  put_char('.'),
  put_code(10), !. % Newline

% Typed literal.
rdf_write_object(literal(type(Datatype,Value1)), _):- !,
  % XSD XML literal.
  (
    rdf_equal(Datatype, rdf:'XMLLiteral')
  ->
    with_output_to(atom(Value2), xml_write(Value1, [header(false)]))
  ;
    Value2 = Value1
  ),

  % Convert numbers to atoms.
  (
    number(Value2)
  ->
    atom_number(Value3, Value2)
  ;
    Value3 = Value2
  ),

  turtle:turtle_write_quoted_string(current_output, Value3),
  write('^^'),
  rdf_write_predicate(Datatype).
% Language-tagged string.
rdf_write_object(literal(lang(Language,Value)), _):- !,
  turtle:turtle_write_quoted_string(current_output, Value),
  format(current_output, '@~w', [Language]).
% XSD string.
rdf_write_object(literal(Value), _):- !,
  turtle:turtle_write_quoted_string(current_output, Value).
% Subject.
rdf_write_object(Term, BNodePrefix):-
  rdf_write_subject(Term, BNodePrefix).


% IRI.
rdf_write_predicate(Iri):-
  turtle:turtle_write_uri(current_output, Iri).


% Blank node.
rdf_write_subject(BNode, BNodePrefix):-
  rdf_is_bnode(BNode), !,
  rdf_write_bnode(BNodePrefix, BNode).
% Predicate.
rdf_write_subject(Iri, _):-
  rdf_write_predicate(Iri).

