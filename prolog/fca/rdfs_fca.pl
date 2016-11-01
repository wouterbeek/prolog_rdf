:- module(
  rdfs_fca,
  [
    rdfs_fca_context/2 % -Context, +G
  ]
).

/** <module> FCA for RDFS

@author Wouter Beek
@version 2015/11-2016/01, 2016/03, 2016/06
*/

:- use_module(library(q/q_term)).

:- rdf_meta
   rdf_fca_context(-, r).





%! rdfs_fca_context(-Context:compound, +G) is det.

rdfs_fca_context(
  context(
    q_term:q_subject(G),
    rdfs_fca:rdfs_class,
    rdfs_fca:rdfs_instance
  ),
  G
).
