:- module(
  rdf_reification,
  [
% DEBUG
    dcg_stmt//3, % +Brackets:oneof([ascii,html])
                 % +Mode:oneof([natlang,triple])
                 % +Statement:iri

% READING
    rdf_object/3, % ?Stmt:statement
                  % ?Object:onef([literal,resource])
                  % +Graph:graph
    rdf_predicate/3, % ?Stmt:statement
                     % ?Predicate:resource
                     % +Graph:graph
    rdf_statement/5, % ?Subject:onef([bnode,literal,resource])
                     % ?Predicate:resource
                     % ?Object:onef([literal,resource])
                     % +Graph:graph
                     % ?Stmt:statement
    rdf_subject/3, % ?Stmt:statement
                   % ?Subject:onef([bnode,literal,resource])
                   % +Graph:graph

% WRITING
    rdf_assert_object/3, % +Stmt:statement
                         % +Object:oneof([literal,resource])
                         % +Graph:graph
    rdf_assert_predicate/3, % +Stmt:statement
                            % +Predicate:resource
                            % +Graph:graph
    rdf_assert_statement/5, % +Subject:resource
                            % +Predicate:resource
                            % +Object:resource
                            % +Graph:graph
                            % -Stmt:statement
    rdf_assert_subject/3 % +Stmt:statement
                         % +Subject:resource
                         % +Graph:graph
  ]
).

/** <module> RDF reification

Reification for RDF. Both reading and writing.

@author Wouter Beek
@tbd Assess this module after reading the semantics standard for reification.
@version 2013/02, 2013/07, 2013/09-2013/10, 2013/12-2014/01, 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_collection)).
:- use_module(dcg(dcg_content)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_object(r,r,?)).
:- rdf_meta(rdf_predicate(r,r,?)).
:- rdf_meta(rdf_statement(r,r,r,?,r)).
:- rdf_meta(rdf_subject(r,r,?)).

:- rdf_meta(rdf_assert_object(r,r,+)).
:- rdf_meta(rdf_assert_predicate(r,r,+)).
:- rdf_meta(rdf_assert_statement(r,r,r,+,r)).
:- rdf_meta(rdf_assert_subject(r,r,+)).



% DEBUG %

%! dcg_stmt(
%!   +Brackets:oneof([ascii,html]),
%!   +Mode:oneof([natlang,triple]),
%!   +Statement:iri
%! )// is det.
% @arg Brackets Either `ascii` or `html`.
% @arg Mode The mode in which the statenent is printed.
%      Either `triple` (default) for an RDF triple representation
%      (according to module RDF_NAME),
%      or `natlang` for a natural language representation.

% Print the natural language representation of the given statement.
dcg_stmt(_, natlang, Stmt) --> !,
  {
    % Retrieve the natural language labels for the subject, object
    % and predicate terms that constitute the statement.
    rdf_statement(S, P, O, _, Stmt),

    % Extract natural language labels for the terms that compose
    % the statement.
    rdfs_label(S, SName),
    rdfs_label(P, PName),
    rdfs_label(O, OName)
  },
  collection(``, ``, =, ` `, atom, [SName,PName,OName]).
% Print the triple representation of the given statement.
dcg_stmt(Brackets, triple, Stmt) -->
  {rdf_statement(S, P, O, _, Stmt)},

  % A statement is serialized as a triple of RDF terms.
  tuple(Brackets, rdf_term_name, [S,P,O]).



% READING %

rdf_object(Stmt, Object, Graph):-
  rdf(Stmt, rdf:object, Object, Graph).

rdf_predicate(Stmt, Predicate, Graph):-
  rdf(Stmt, rdf:predicate, Predicate, Graph).

rdf_statement(Subject, Predicate, Object, Graph, Stmt):-
  rdf_subject(Stmt, Subject, Graph),
  rdf_predicate(Stmt, Predicate, Graph),
  rdf_object(Stmt, Object, Graph).

rdf_subject(Stmt, Subject, Graph):-
  rdf(Stmt, rdf:subject, Subject, Graph).



% WRITING %

rdf_assert_object(Stmt, Object, Graph):-
  rdf_assert(Stmt, rdf:object, Object, Graph).

rdf_assert_predicate(Stmt, Predicate, Graph):-
  rdf_assert(Stmt, rdf:predicate, Predicate, Graph).

rdf_assert_statement(Subject, Predicate, Object, Graph, Stmt):-
  rdf_statement(Subject, Predicate, Object, Graph, Stmt), !.
rdf_assert_statement(Subject, Predicate, Object, Graph, Stmt):-
  % Make sure the statement parameter is instantiated.
  % Use a new blank node if this is not yet the case.
  (
    var(Stmt)
  ->
    rdf_bnode(Stmt)
  ;
    true
  ),
  
  rdf_assert_individual(Stmt, rdf:'Statement', Graph),
  rdf_assert_subject(Stmt, Subject, Graph),
  rdf_assert_predicate(Stmt, Predicate, Graph),
  rdf_assert_object(Stmt, Object, Graph),
  rdf_assert(Subject, Predicate, Object, Graph).

rdf_assert_subject(Stmt, Subject, Graph):-
  rdf_assert(Stmt, rdf:subject, Subject, Graph).

