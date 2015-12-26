:- module(
  rdf_image,
  [
    rdf_image/3, % ?Subject, ?Predicate, ?Object
    rdf_image/4 %?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Image:iri
                % ?Graph:atom
  ]
).

/** <module> RDF image

@author Wouter Beek
@version 2015/07-2015/08, 2015/12
*/

:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdfs/rdfs_read)).

:- rdf_meta(rdf_image(r,r,o)).
:- rdf_meta(rdf_image(r,r,o,r)).





%! rdf_image(?Subject:rdf_term, ?Predicate:iri, ?Image:iri) is nondet.

rdf_image(S, P, O):-
  rdf_image(S, P, O, _).


%! rdf_image(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Image:iri,
%!   ?Graph:rdf_graph
%! ) is nondet.

rdf_image(S, P, O, G):-
  (   rdf_member(P, [dbo:thumbnail,foaf:depiction]),
      rdf(S, P, O, G)
  ;   rdf(S, P, O, G),
      rdfs_instance(O, dcmit:'Image')
  ).
