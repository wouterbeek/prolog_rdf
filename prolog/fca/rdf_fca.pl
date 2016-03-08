:- module(
  rdf_fca,
  [
    rdf_fca_context/2 % -Context, +G
  ]
).

/** <module> FCA for RDF

@author Wouter Beek
@version 2015/11-2016/01, 2016/03
*/

:- use_module(library(semweb/rdf11)).

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
