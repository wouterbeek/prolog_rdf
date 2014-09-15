:- module(
  rdf_reification_write,
  [
    rdf_assert_datatype_statement/6, % +Subject:or([bnode,iri])
                                     % +Predicate:iri
                                     % +Value
                                     % +Datatype:iri
                                     % +Graph:atom
                                     % -Statement:or([bnode,iri])
    rdf_assert_object/3, % +Statement:or([bnode,iri])
                         % +Object:oneof([literal,iri])
                         % +Graph:atom
    rdf_assert_predicate/3, % +Statement:or([bnode,iri])
                            % +Predicate:iri
                            % +Graph:atom
    rdf_assert_statement/3, % +Triple:compound
                            % +Graph:atom
                            % ?Statement:or([bnode,iri])
    rdf_assert_subject/3 % +Statement:or([bnode,iri])
                         % +Subject:iri
                         % +Graph:atom
  ]
).

/** <module> RDF reification: read

Read support for reified triples.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plXsd(xsd)).

:- use_module(plRdf_term(rdf_literal)).
:- use_module(plRdf_term(rdf_literal_build)).

:- rdf_meta(rdf_assert_datatype_statement(r,r,+,r,+,?)).
:- rdf_meta(rdf_assert_object(r,o,+)).
:- rdf_meta(rdf_assert_predicate(r,r,+)).
:- rdf_meta(rdf_assert_statement(t,+,?)).
:- rdf_meta(rdf_assert_subject(r,r,+)).



%! rdf_assert_datatype_statement(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   +Datatype:iri,
%!   +Graph:graph,
%!   ?Statement:or(bnode,iri])
%! ) is det.
% Asserts a datatyped statement, and automatically converts the given value
% to its corresponding lexical form.

rdf_assert_datatype_statement(
  Subject,
  Predicate,
  Value,
  Datatype0,
  Graph,
  Statement
):-
  % RDF 1.1 states that untyped literals have type `xsd:string`.
  (   var(Datatype0)
  ->  rdf_global_id(xsd:string, Datatype)
  ;   Datatype = Datatype0
  ),
  
  xsd_canonical_map(Datatype, Value, LexicalForm),
  rdf_literal(Literal, LexicalForm, Datatype),
  rdf_assert_statement(rdf(Subject,Predicate,Literal), Graph, Statement).


%! rdf_assert_object(
%!   +Statement:or([bnode,iri]),
%!   +Object:or([bnode,iri,literal]),
%!   +Graph:atom
%! ) is det.

rdf_assert_object(Statement, Object, Graph):-
  rdf_is_literal(Object), !,
  rdf_literal(Object, LexicalForm, Datatype, LanguageTag),
  rdf_assert_literal(
    Statement,
    rdf:object,
    LexicalForm,
    Datatype,
    LanguageTag,
    Graph
  ).
rdf_assert_object(Statement, Object, Graph):-
  rdf_assert(Statement, rdf:object, Object, Graph).


%! rdf_assert_predicate(
%!   +Statement:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Graph:atom
%! ) is det.

rdf_assert_predicate(Statement, Predicate, Graph):-
  rdf_assert(Statement, rdf:predicate, Predicate, Graph).


%! rdf_assert_statement(
%!   +Triple:compound,
%!   +Graph:atom,
%!   ?Statement:or([bnode,iri])
%! ) is det.

rdf_assert_statement(rdf(Subject,Predicate,Object), Graph, Statement):-
  rdf_statement(Subject, Predicate, Object, Graph, Statement), !.
rdf_assert_statement(rdf(Subject,Predicate,Object), Graph, Statement):-
  % Make sure the statement parameter is instantiated.
  % Use a new blank node if this is not yet the case.
  (   var(Statement)
  ->  rdf_bnode(Statement)
  ;   true
  ),

  rdf_assert_instance(Statement, rdf:'Statement', Graph),
  rdf_assert_subject(Statement, Subject, Graph),
  rdf_assert_predicate(Statement, Predicate, Graph),
  rdf_assert_object(Statement, Object, Graph),
  rdf_assert(Subject, Predicate, Object, Graph).


%! rdf_assert_subject(
%!   +Statement:or([bnode,iri]),
%!   +Subject:or([bnode,iri]),
%!   +Graph:atom
%! ) is det.

rdf_assert_subject(Statement, Subject, Graph):-
  rdf_assert(Statement, rdf:subject, Subject, Graph).

