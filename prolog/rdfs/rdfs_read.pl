:- module(
  rdfs_read,
  [
    rdfs_label/2, % ?Subject:or([bnode,iri])
                  % ?Value
    rdfs_label/3 % ?Subject:or([bnode,iri])
                 % ?Value
                 % ?Graph:atom
  ]
).

/** <module> RDFS read

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdfs_label(r,?)).
:- rdf_meta(rdfs_label(r,?,?)).





%! rdfs_label(?Subject:or([bnode,iri]), ?Value) is nondet.
% Wrapper around rdfs_label/3.

rdfs_label(S, V):-
  rdfs_label(S, V, _).



%! rdfs_label(?Subject:or([bnode,iri]), ?Value, ?Graph:atom) is nondet.
% Reads RDFS labels attributed to resources.

rdfs_label(S, V, G):-
  rdf_literal(S, rdfs:label, V, G).
