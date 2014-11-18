:- module(
  rdf_reification_print,
  [
    dcg_stmt//3 % +Brackets:oneof([ascii,html])
                % +Mode:oneof([natlang,triple])
                % +Statement:or([bnode,iri])
  ]
).

/** <module> RDF reification: print

Print reified RDF statements.

@author Wouter Beek
@version 2013/02, 2013/07, 2013/09-2013/10, 2013/12-2014/01, 2014/03, 2014/06,
         2014/09-2014/10
*/

:- use_module(library(semweb/rdfs)).

:- use_module(plDcg(dcg_atom)).
:- use_module(plDcg(dcg_collection)).
:- use_module(plDcg(dcg_content)).

:- use_module(plRdf(rdf_name)).
:- use_module(plRdf(reification/rdf_reification_read)).



%! dcg_stmt(
%!   +Brackets:oneof([ascii,html]),
%!   +Mode:oneof([natlang,triple]),
%!   +Statement:or([bnode,iri])
%! )// is det.
% @arg Brackets Either `ascii` or `html`.
% @arg Mode The mode in which the statenent is printed.
%      Either `triple` (default) for an RDF triple representation
%      (according to module RDF_NAME),
%      or `natlang` for a natural language representation.

% Print the natural language representation of the given statement.
dcg_stmt(_, natlang, Statement) --> !,
  {
    % Retrieve the natural language labels for the subject, object
    % and predicate terms that constitute the statement.
    rdf_statement(S, P, O, _, Statement),

    % Extract natural language labels for the terms that compose
    % the statement.
    rdfs_label(S, SName),
    rdfs_label(P, PName),
    rdfs_label(O, OName)
  },
  collection(``, ``, =, ` `, atom, [SName,PName,OName]).
% Print the triple representation of the given statement.
dcg_stmt(Brackets, triple, Statement) -->
  {rdf_statement(S, P, O, _, Statement)},

  % A statement is serialized as a triple of RDF terms.
  tuple(Brackets, rdf_term_name, [S,P,O]).

