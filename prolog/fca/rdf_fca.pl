:- module(
  rdf_fca,
  [
    rdf_fca_context/2 % -Context:compound
                      % +Graph:rdf_graph
  ]
).

/** <module> FCA for RDF

@author Wouter Beek
@version 2015/11-2015/12
*/

:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdfs/rdfs_read)).

:- rdf_meta(rdf_fca_context(-,r)).





%! rdf_fca_context(-Context:compound, +Graph:rdf_graph) is det.

rdf_fca_context(
  context(
    rdf_term:rdf_subject(G),
    rdf_fca:rdfs_class0(G),
    rdf_fca:rdf_instance0(G)
  ),
  G
).
rdfs_class0(G, A):- rdfs_class(A, G).
rdf_instance0(G, O, A):- rdf_instance(O, A, G).
