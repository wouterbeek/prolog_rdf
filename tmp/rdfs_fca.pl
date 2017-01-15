:- module(
  rdfs_fca,
  [
    rdfs_fca_context/2 % -Context, +G
  ]
).

/** <module> FCA for RDFS

@author Wouter Beek
@version 2015/11-2016/01, 2016/03, 2016/06, 2016/12
*/

:- use_module(library(rdf/rdf_term)).

:- rdf_meta
   rdfs_fca_context(-, r).





%! rdfs_fca_context(-Context:compound, +G) is det.

rdfs_fca_context(
  context(
    rdf_term:rdf_subject(G),
    rdfs_fca:rdfs_class,
    rdfs_fca:rdfs_instance
  ),
  G
).
