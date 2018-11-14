:- module(
  rdf_triple,
  [
    rdf_triple_term/2 % +Triple, ?Term
  ]
).

/** <module> RDF triple

@author Wouter Beek
@version 2018
*/

:- use_module(library(semweb/rdf_prefix)).

:- rdf_meta
   rdf_triple_term(t, r).





%! rdf_triple_term(+Triple:rdf_triple, +Term:rdf_term) is semidet.
%! rdf_triple_term(+Triple:rdf_triple, -Term:rdf_term) is multi.

rdf_triple_term(tp(S,_,_), S).
rdf_triple_term(tp(_,P,_), P).
rdf_triple_term(tp(_,_,O), O).
