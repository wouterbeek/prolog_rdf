:- module(rdf_database, [
     rdf_estimate_complexity/4 % ?Subject:rdf_term
                               % ?Predicate:iri
                               % ?Object:rdf_term
                               % -Complexity:nonneg
   ]).
:- reexport(library(semweb/rdf_db), [
     rdf_active_transaction/1, % ?Id
     rdf_current_snapshot/1, % ?Term
     rdf_delete_snapshot/1, % +Snapshot
     rdf_gc/0,
     rdf_generation/1, % -Generation
     rdf_monitor/2, % :Goal_0
                    % +Mask
     rdf_reset_db/0,
     rdf_snapshot/1, % -Snapshot
     rdf_transaction/1, % :Goal_0
     rdf_transaction/2, % :Goal_0, +Id
     rdf_transaction/3, % :Goal_0
                        % +Id
                        % +Options:list(compound)
     rdf_update_duplicates/0,
     rdf_warm_indexes/0,
     rdf_warm_indexes/1 % +Indexes
   ]).
:- reexport(library(semweb/rdf_persistency), [
     rdf_attach_db/2 % +Directory:atom
                     % +Options:list(compound)
   ]).

/** <module> RDF database

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db), [
     rdf_estimate_complexity/4 as rdf_estimate_complexity0
   ]).

:- rdf_meta(rdf_estimate_complexity(o,r,o,-)).





%! rdf_estimate_complexity(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   -Complexity:nonneg
%! ) is det.

rdf_estimate_complexity(S, P, O, N) :-
  rdf(S, P, O, Sid, Pid, Oid),
  rdf_estimate_complexity0(Sid, Pid, Oid, N), !.
rdf_estimate_complexity(_, _, _, 0).
