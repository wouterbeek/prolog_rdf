:- module(
  rdf_ntriples_write,
  [
    rdf_ntriples_write/2 % +Write:or([atom,stream])
                         % +Options:list
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
@version 2014/03-2014/05
*/

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)). % Private predicates.
:- use_module(library(sgml_write)).
:- use_module(library(uri)).

:- thread_local(bnode_counter/1).
:- thread_local(bnode_map/2).



%! rdf_ntriples_write(+Write:or([atom,stream]), +Options:list) is det.
% Writes RDF data serialization in the N-Triples format to the given file.
%
% The following options are supported:
%   * =|bnode_base(?Iri:atom)|=
%     Replace blank nodes with an IRI, defined as per
%     RDF 1.1 spec (see link below).
%   * =|graph(?Graph:atom)|=
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

  % Reset the blank node store.
  reset_bnode_admin,

  % Keep track of the number of triples written.
  State = state(0),

  % Process the option for replacing blank nodes with IRIs,
  % establishing the prefix for each blank node.
  (
    option(bnode_base(Iri), Options)
  ->
    uri_components(Iri, uri_components(Scheme,Authority,_,_,_)),
    uri_components(IriPrefix, uri_components(Scheme,Authority,_,_,_)),
    atom_concat(IriPrefix, IriPostfix, Iri),
    rdf_atom_md5(IriPostfix, 1, Hash1),
    atomic_concat(Hash1, '#', Hash2),
    atomic_list_concat(['','.well-known',genid,Hash2], '/', Path),
    uri_components(
      BNodePrefix,
      uri_components(Scheme,Authority,Path,_,_)
    )
  ;
    BNodePrefix = '_:'
  ),
  (
    option(graph(Graph), Options)
  ->
    forall(
      rdf(S, P, O, Graph:_),
      (
        inc_number_of_triples(State),
        rdf_write_ntriple(Write, S, P, O, BNodePrefix)
      )
    )
  ;
    forall(
      % Avoid duplicate triples.
      rdf(S, P, O),
      (
        inc_number_of_triples(State),
        rdf_write_ntriple(Write, S, P, O, BNodePrefix)
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
% Input is a file.
% Open a stream in `write` mode.
rdf_ntriples_write(File, Options):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, write, Write),
    rdf_ntriples_write(Write, Options),
    close(Write)
  ).

inc_number_of_triples(State) :-
  arg(1, State, C0),
  C1 is C0 + 1,
  nb_setarg(1, State, C1).

rdf_write_ntriple(Write, S, P, O, BNodePrefix):-
  flag(number_of_ntriples, X, X + 1),
  rdf_write_subject(Write, S, BNodePrefix),
  put_char(Write, ' '),
  rdf_write_predicate(Write, P),
  put_char(Write, ' '),
  rdf_write_object(Write, O, BNodePrefix),
  put_char(Write, ' '),
  put_char(Write, '.'),
  put_code(Write, 10), !. % Newline

% Typed literal.
rdf_write_object(Write, literal(type(Datatype,Value1)), _):- !,
  % XSD XML literal.
  (
    rdf_equal(Datatype, rdf:'XMLLiteral')
  ->
    with_output_to(atom(Value2), xml_write(Value1, [header(false)]))
  ;
    Value2 = Value1
  ),

  % Convert numbers to atom.
  (
    number(Value2)
  ->
    atom_number(Value3, Value2)
  ;
    Value3 = Value2
  ),

  turtle:turtle_write_quoted_string(Write, Value3),
  write(Write, '^^'),
  rdf_write_predicate(Write, Datatype).
% Language-tagged string.
rdf_write_object(Write, literal(lang(Language,Value)), _):- !,
  turtle:turtle_write_quoted_string(Write, Value),
  format(Write, '@~w', [Language]).
% XSD string.
rdf_write_object(Write, literal(Value), _):- !,
  turtle:turtle_write_quoted_string(Write, Value).
% Subject.
rdf_write_object(Write, Term, BNodePrefix):-
  rdf_write_subject(Write, Term, BNodePrefix).



% IRI.
rdf_write_predicate(Write, Iri):-
  turtle:turtle_write_uri(Write, Iri).


% Blank node.
rdf_write_subject(Write, BNode, BNodePrefix):-
  rdf_is_bnode(BNode), !,
  (
    bnode_map(BNode, Id2)
  ->
    true
  ;
    increment_bnode_counter(Id2),
    assert(bnode_map(BNode, Id2))
  ),
  atomic_concat(BNodePrefix, Id2, BNodeName),

  % If the blank node is replaced by a well-known IRI,
  % then we use predicate term writer.
  (
    BNodePrefix == '_:'
  ->
    write(Write, BNodeName)
  ;
    rdf_write_predicate(Write, BNodeName)
  ).
% Predicate.
rdf_write_subject(Write, Iri, _):-
  rdf_write_predicate(Write, Iri).



% Blank node administration.

increment_bnode_counter(Id2):-
  retract(bnode_counter(Id1)),
  Id2 is Id1 + 1,
  assert(bnode_counter(Id2)).

reset_bnode_admin:-
  reset_bnode_counter,
  reset_bnode_map.

reset_bnode_counter:-
  retractall(bnode_counter(_)),
  assert(bnode_counter(0)).

reset_bnode_map:-
  retractall(bnode_map(_,_)).

