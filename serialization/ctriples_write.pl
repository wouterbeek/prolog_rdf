:- module(
  ctriples_write,
  [
    ctriples_write/2, % +Write:or([atom,stream])
                      % +Options:list(nvpair)
    ctriples_write/3, % +Write:or([atom,stream])
                      % +Triples:list(compound)
                      % +Options:list(nvpair)
    ctriples_write_to_stream/2 % +Triples:list(compound)
                               % +Options:list(nvpair)
  ]
).

/** <module> Serialize RDF in the C-Triples and C-Quads format

A simple implementation for emitting RDF data in
C-Triples and C-Quads serialization formats,
reusing some of the Turtle writer.
Intended to work with RDF data stored using SWI-Prolog's Semweb library.

In C-Triples/C-Quads only short Turtle strings
(delimited by a single double quote) occur.
Linefeeds and carriage returns are escaped.
This means that we can guarantee that the number of triples
is the same as the number of lines in the generated file.

Also the number of spaces between the RDF terms is exactly one
and lines end in one space, one dot, and a newline character.

Simple literals are not emitted, the `xsd:string` datatype is made explicit.
Language-tagged strings are made explicit with datatype `rdf:langString`.

@author Wouter Beek
@author Jan Wielemaker
@author Laurens Rietveld
@compat http://www.w3.org/TR/2014/REC-n-triples-20140225/
@version 2014/03-2014/06, 2014/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)). % Private predicates.
:- use_module(library(sgml_write)).

:- use_module(generics(typecheck)).

:- use_module(plRdf(rdf_graph)).
:- use_module(plRdf_ser(rdf_bnode_write)).
:- use_module(plRdf_term(rdf_term)).

:- thread_local(ctriples_format/1).



%! ctriples_write(+Source:or([atom,stream]), +Options:list) is det.
% `Source` is either a file name or a write stream.
%
% The following options are supported:
%   * =|bnode_base(+atom)|=
%     Replace blank nodes with an IRI, defined as per
%     RDF 1.1 spec (see link below).
%   * =|format(-oneof([quads,triples]))|=
%     `quads` if at least one named graph occurs, `triples` otherwise.
%   * =|graph(+atom)|=
%     The atomic name of a currently loaded RDF graph,
%     to restrict the triples that are saved,
%     or uninstantiated, in which case
%     all currently loaded triples are saved.
%   * =|number_of_triples(-nonneg)|=
%     The number of triples that was written.
%
% @see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-skolemization

% Input is a stream.
ctriples_write(Write, Options):-
  is_stream(Write), !,
  with_output_to(Write, ctriples_write_to_stream(Options)).
% Input is a file.
% Open a stream in `write` mode.
ctriples_write(File, Options):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, write, Write),
    with_output_to(Write, ctriples_write_to_stream(Options)),
    close(Write)
  ).


%! ctriples_write(
%!   +Source:or([atom,stream]),
%!   +Triples:list(compound),
%!   +Options:list
%! ) is det.
% Writes RDF data using the C-Triples/C-Quads serialization format.

ctriples_write(Write, Triples, Options):-
  is_stream(Write), !,
  with_output_to(Write, ctriples_write_to_stream(Triples, Options)).
ctriples_write(Write, Triples, Options):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, write, Write),
    with_output_to(Write, ctriples_write_to_stream(Triples, Options)),
    close(Write)
  ).


%! ctriples_write_to_stream(+Options:list(nvpair)) is det.
% This assumes that we can write to current output.

ctriples_write_to_stream(Options):-
  ctriples_write_to_stream_begin(Options, State, BNodePrefix),
    % Whether triples are read from a specific graph or not.
    option(graph(Graph), Options, _VAR),
    ctriples_write_triples(State, BNodePrefix, Graph),
  ctriples_write_to_stream_end(Options, State).

%! ctriples_write_to_stream(
%!   +Triples:list(compound),
%!   +Options:list(nvpair)
%! ) is det.

ctriples_write_to_stream(Triples1, Options):-
  ctriples_write_to_stream_begin(Options, State, BNodePrefix),
    sort(Triples1, Triples2),
    maplist(ctriples_write_triple(State, BNodePrefix), Triples2),
  ctriples_write_to_stream_end(Options, State).

%! ctriples_write_to_stream_begin(
%!   +Options:list(nvpair),
%!   -State:compound,
%!   -BNodePrefix:iri
%! ) is det.

ctriples_write_to_stream_begin(Options, State, BNodePrefix):-
  reset_bnode_admin,
  reset_ctriples_format,

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
  ).

%! ctriples_write_to_stream_end(
%!   +Options:list(nvpair),
%!   +State:compound
%! ) is det.

ctriples_write_to_stream_end(Options, State):-
  % The serialization format: triples or quads?
  (
    option(format(Format), Options)
  ->
    ctriples_format(Format)
  ;
    true
  ),

  % Statistics option: number of triples written.
  (
    option(number_of_triples(TripleCount), Options)
  ->
    arg(1, State, TripleCount)
  ;
    true
  ).


%! ctriples_write_triples(
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   ?Graph:atom
%! ) is det.
% Writes either all triples or triples that occur in the given graph.
%
% Collects a sorted list of subjects terms.
% Then processes each subject term separately.

ctriples_write_triples(State, BNodePrefix, Graph):-
  sorted_subject_terms(Graph, Subjects),
  maplist(ctriples_write_subject(State, BNodePrefix, Graph), Subjects).


%! ctriples_write_subject(
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   +Graph:atom,
%!   +Subject:or([bnode,iri])
%! ) is det.
% Writes all triples that occur with the given subject term,
% possibly restricted to a given graph.
%
% Collects a sorted list of predicate-object pairs.
% Then processes each pairs -- and thus each triple -- separately.

ctriples_write_subject(State, BNodePrefix, G, S):-
  % Collect a sorted list of the predicate-object pairs
  % for the given subject term.
  aggregate_all(
    set(P-O-G),
    rdf(S, P, O, G:_),
    POPairs
  ),
  maplist(ctriples_write_triple(State, BNodePrefix, S), POPairs).


%! ctriples_write_triple0(+BNodePrefix:iri, +Triple:compound) is det.

ctriples_write_triple(BNodePrefix, rdf(S,P,O)):- !,
  rdf_write_ctriple(S, P, O, _, BNodePrefix).
ctriples_write_triple(BNodePrefix, rdf(S,P,O,G)):-
  rdf_write_ctriple(S, P, O, G, BNodePrefix).


%! ctriples_write_triple(
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   +Triple:list(compound)
%! ) is det.

ctriples_write_triple(State, BNodePrefix, Triple):-
  inc_number_of_triples(State),
  ctriples_write_triple(BNodePrefix, Triple).


%! ctriples_write_triple(
%!   +State:compound,
%!   +BNodePrefix:iri,
%!   +Subject:or([bnode,iri]),
%!   +PredicateObjectPairs:ordset(triple(iri,or([bnode,iri,literal]),iri))
%! ) is det.
% Writes a single triple.

ctriples_write_triple(State, BNodePrefix, S, P-O-G):-
  inc_number_of_triples(State),
  rdf_write_ctriple(S, P, O, G, BNodePrefix).


inc_number_of_triples(State) :-
  arg(1, State, C0),
  C1 is C0 + 1,
  nb_setarg(1, State, C1).


%! rdf_write_ctriple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   ?Graph:iri,
%!   +BNodePrefix:atom
%! ) is det.

rdf_write_ctriple(S, P, O, Graph, BNodePrefix):-
  rdf_write_subject(S, BNodePrefix),
  put_char(' '),
  rdf_write_predicate(P),
  put_char(' '),
  rdf_write_object(O, BNodePrefix),
  put_char(' '),

  % C-Triples or C-Quads?
  (
    is_url(Graph)
  ->
    % The format is now C-Quads.
    set_ctriples_format_to_quads,

    % Named graphs and predicate terms are both IRIs.
    rdf_write_predicate(Graph),
    put_char(' ')
  ;
    true
  ),

  % End of triple / end of line.
  put_char('.'),
  put_code(10), !. % Newline character.


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
  rdf_bnode_write(BNodePrefix, BNode).
% Predicate.
rdf_write_subject(Iri, _):-
  rdf_write_predicate(Iri).


%! reset_ctriples_format is det.

reset_ctriples_format:-
  retractall(ctriples_format(_)),
  assert(ctriples_format(triples)).

%! set_ctriples_format_to_quads is det.

set_ctriples_format_to_quads:-
  ctriples_format(quads), !.
set_ctriples_format_to_quads:-
  assert(ctriples_format(quads)).


%! sorted_subject_terms(?Graph:atom, -Subjects:ordset(or([bnode,iri]))) is det.
% Returns a sorted list of subject terms, possibly resricted to a given graph.

sorted_subject_terms(Graph, Subjects):-
  is_rdf_graph(Graph), !,
  aggregate_all(
    set(Subject),
    rdf_subject(Subject, Graph),
    Subjects
  ).
sorted_subject_terms(_, Subjects):-
  aggregate_all(
    set(Subject),
    rdf_subject(Subject),
    Subjects
  ).

