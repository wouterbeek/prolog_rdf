:- module(rdf_db, []).
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
     rdf_update_duplicates/0
   ]).
