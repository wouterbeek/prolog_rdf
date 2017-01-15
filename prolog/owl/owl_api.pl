:- module(
  owl_api,
  [
    owl_identity/4 % +M, ?I, ?J, ?G
  ]
).
:- reexport(library(rdfs/rdfs_api)).

/** <module> OWL read API

@author Wouter Beek
@version 2017/01
*/

:- rdf_meta
   owl_identity(?, r, r, r).





%! owl_identity(?M, ?I, ?J, ?G) is nondet.

owl_identity(M, I, J, G) :-
  t(M, I, owl:sameAs, J, G).
