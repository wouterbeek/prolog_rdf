:- module(
  rdfs_read,
  [
    rdfs_instance/2 % ?Instance:rdf_term
                    % ?Class:or([bnode,iri])
  ]
).
:- reexport(library(rdf/rdf_read)).
:- reexport(library(semweb/rdfs)).

/** <module> RDFS read

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(rdf/rdf_build)).

:- rdf_meta(rdfs_instance(o,r)).





%! rdfs_instance(?Instance:rdf_term, ?Class:or([bnode,iri])) is nondet.

rdfs_instance(I0, C):-
  rdf_is_literal(I0), !,
  subject_literal(I0, I),
  rdfs_instance(I, C).
rdfs_instance(I, C):-
  rdfs_individual_of(I, C).
