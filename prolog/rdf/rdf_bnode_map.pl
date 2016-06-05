:- module(
  rdf_bnode_map,
  [
    rdf_bnode_map/2 % +BNode, -Name
  ]
).

/** <module> RDF blank node map

@author Wouter Beek
@version 2016/06
*/

:- dynamic
    rdf_bnode_map0/2.





%! rdf_bnode_map(+B, -Name) is det.

rdf_bnode_map(B, Name) :-
  rdf_bnode_map0(B, Name), !.
rdf_bnode_map(B, Name) :-
  flag(bnode_counter, Name0, Name0 + 1),
  Name is Name0 + 1,
  assert(rdf_bnode_map0(B, Name)).
