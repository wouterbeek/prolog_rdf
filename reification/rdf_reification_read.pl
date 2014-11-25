:- module(
  rdf_reification_read,
  [
    rdf_literal_statement/6, % ?Subject:or([bnode,iri])
                              % ?Predicate:iri
                              % ?Value
                              % ?Datatype:iri
                              % ?Graph:graph
                              % ?Statement:or([bnode,iri])
    rdf_object/3, % ?Statement:or([bnode,iri])
                  % ?Object:rdf_term
                  % ?Graph:graph
    rdf_predicate/3, % ?Statement:or([bnode,iri])
                     % ?Predicate:iri
                     % +Graph:atom
    rdf_statement/5, % ?Subject:onef([bnode,iri])
                     % ?Predicate:iri
                     % ?Object:rdf_term
                     % ?Graph:atom
                     % ?Statement:or([bnode,iri])
    rdf_subject/3 % ?Statement:or([bnode,iri])
                  % ?Subject:or([bnode,iri])
                  % ?Graph:atom
  ]
).

/** <module> RDF reification: read

Read support for reified triples.

@author Wouter Beek
@version 2014/09-2014/10
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plXsd(xsd)).

:- use_module(plRdf(api/rdf_read)).

:- rdf_meta(rdf_literal_statement(r,r,?,r,?,r)).
:- rdf_meta(rdf_object(r,o,?)).
:- rdf_meta(rdf_predicate(r,r,?)).
:- rdf_meta(rdf_statement(r,r,o,?,r)).
:- rdf_meta(rdf_subject(r,r,?)).



%! rdf_literal_statement(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?Datatype:iri,
%!   ?Graph:atom,
%!   ?Statement:or([bnode,iri])
%! ) is nondet.
% Reads statements with datatyped literals converted to proper values.

rdf_literal_statement(S, P, Value, Datatype, Graph, Statement):-
  rdf_subject(Statement, S, Graph),
  rdf_predicate(Statement, P, Graph),
  rdf_literal(S, P, Value, Datatype, _, Graph, rdf(S,P,O)),
  rdf_object(Statement, O, Graph).



%! rdf_object(
%!   ?Statement:or([bnode,iri]),
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.

% Literals are treated specially.
rdf_object(Statement, O, Graph):-
  rdf(Statement, rdf:object, O, Graph).



%! rdf_predicate(
%!   ?Statement:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Graph:atom
%! ) is nondet.

rdf_predicate(Statement, P, Graph):-
  rdf(Statement, rdf:predicate, P, Graph).



%! rdf_statement(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   ?Statement:or([bnode,iri])
%! ) is nondet.
% Relations between statements and their components,
% where the object term is treated as a plain literal,
% i.e. not interpreted.

rdf_statement(S, P, O, Graph, Statement):-
  rdf_subject(Statement, S, Graph),
  rdf_predicate(Statement, P, Graph),
  rdf_object(Statement, O, Graph).



%! rdf_subject(
%!   ?Statement:or([bnode,iri]),
%!   ?Subject:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.

rdf_subject(Statement, S, Graph):-
  rdf(Statement, rdf:subject, S, Graph).

