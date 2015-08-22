:- module(
  rdfs_read,
  [
    rdfs_comment/2, % ?Subject:rdf_term
                    % ?Comment:atom
    rdfs_instance/2 % ?Instance:rdf_term
                    % ?Class:or([bnode,iri])
  ]
).

/** <module> RDFS read

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(owl/owl_read)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta(rdfs_comment(o,?)).
:- rdf_meta(rdfs_instance(o,r)).





%! rdfs_comment(?Subject:rdf_term, ?Comment:atom) is nondet.

rdfs_comment(S, Comm):-
  rdf_literal(S0, rdfs:comment, xsd:string, Comm),
  owl_id(S, S0).



%! rdfs_instance(?Instance:rdf_term, ?Class:or([bnode,iri])) is nondet.

rdfs_instance(I0, C):-
  rdf_is_literal(I0), !,
  subject_literal(I0, I),
  rdfs_instance(I, C).
rdfs_instance(I, C):-
  rdfs_individual_of(I, C).
