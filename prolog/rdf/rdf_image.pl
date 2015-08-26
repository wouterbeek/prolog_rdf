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
@version 2015/07-2015/08
*/

:- use_module(library(owl/owl_read)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdfs/rdfs_read)).

:- rdf_register_prefix(dbo, 'http://dbpedia.org/ontology/').
:- rdf_register_prefix(dcmit, 'http://purl.org/dc/dcmitype/').
:- rdf_register_prefix(foaf, 'http://xmlns.com/foaf/0.1/').

:- rdf_meta(rdf_image(r,r,o)).
:- rdf_meta(rdf_image(r,r,o,?)).





%! rdf_image(?Subject:or([bnode,iri]), ?Predicate:iri, ?Image:iri) is nondet.

rdf_image(S, P, O):-
  rdf_image(S, P, O, _).


%! rdf_image(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Image:iri,
%!   ?Graph:atom
%! ) is nondet.

rdf_image(S, P, O, G):-
  owl_id(S, S0),
  (   rdf_member(P, [dbo:thumbnail,foaf:depiction]),
      rdf2(S0, P, O, G)
  ;   rdf2(S0, P, O, G),
      rdfs_instance(O, dcmit:'Image')
  ).
