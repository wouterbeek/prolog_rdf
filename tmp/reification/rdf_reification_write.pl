:- module(
  rdf_reification_write,
  [
    rdf_assert_literal_statement/6, % +Subject:or([bnode,iri])
                                    % +Predicate:iri
                                    % +Value
                                    % +Datatype:iri
                                    % ?Graph:atom
                                    % ?Statement:or([bnode,iri])
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
@version 2014/09, 2014/11
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(reification/rdf_reification_read)).

:- rdf_meta(rdf_assert_datatype_statement(r,r,+,r,?,r)).
:- rdf_meta(rdf_assert_object(r,o,?)).
:- rdf_meta(rdf_assert_predicate(r,r,?)).
:- rdf_meta(rdf_assert_statement(t,?,r)).
:- rdf_meta(rdf_assert_subject(r,r,?)).





%! rdf_assert_literal_statement(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   +Datatype:iri,
%!   ?Graph:graph,
%!   ?Statement:or([bnode,iri])
%! ) is det.
% Asserts a datatyped statement, automatically converting the given value
%  to its corresponding lexical form.

rdf_assert_literal_statement(S, P, Value, Datatype, G, Statement):-
  rdf_assert_literal(S, P, Value, Datatype, G, Triple),
  rdf_assert_statement(Triple, G, Statement).



%! rdf_assert_object(
%!   +Statement:or([bnode,iri]),
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.

rdf_assert_object(Statement, O, Graph):-
  user:rdf_assert(Statement, rdf:object, O, Graph).



%! rdf_assert_predicate(
%!   +Statement:or([bnode,iri]),
%!   +Predicate:iri,
%!   ?Graph:atom
%! ) is det.

rdf_assert_predicate(Statement, P, Graph):-
  user:rdf_assert(Statement, rdf:predicate, P, Graph).



%! rdf_assert_statement(
%!   +Triple:compound,
%!   ?Graph:atom,
%!   ?Statement:or([bnode,iri])
%! ) is det.

rdf_assert_statement(rdf(S,P,O), Graph, Statement):-
  rdf_statement(S, P, O, Graph, Statement), !.
rdf_assert_statement(rdf(S,P,O), Graph, Statement):-
  % Make sure the statement parameter is instantiated.
  % Use a new blank node if this is not yet the case.
  (var(Statement) -> rdf_bnode(Statement) ; true),

  rdf_assert_instance(Statement, rdf:'Statement', Graph),
  rdf_assert_subject(Statement, S, Graph),
  rdf_assert_predicate(Statement, P, Graph),
  rdf_assert_object(Statement, O, Graph),
  user:rdf_assert(S, P, O, Graph).



%! rdf_assert_subject(
%!   +Statement:or([bnode,iri]),
%!   +Subject:or([bnode,iri]),
%!   +Graph:atom
%! ) is det.

rdf_assert_subject(Statement, S, Graph):-
  user:rdf_assert(Statement, rdf:subject, S, Graph).
