:- module(
  rdf_fca,
  [
    rdf_fca_context/2 % -Context, +G
  ]
).

/** <module> FCA for RDF

@author Wouter Beek
@version 2015/11-2016/01
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(rdf11/rdf11_mt)).

:- rdf_meta
   rdf_fca_context(-, r).





%! rdf_fca_context(-Context:compound, +G) is det.

rdf_fca_context(
  context(
    rdf_term:rdf_subject(G),
    rdf_fca:rdfs_class,
    rdf_fca:rdfs_instance
  ),
  G
).
