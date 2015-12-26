:- module(
  rdf_image,
  [
    rdf_image/3, % ?Subject, ?Predicate, ?Image
    rdf_image/4 %?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Image:atom
                % ?Graph:rdf_graph
  ]
).

/** <module> RDF: Read image assertions

@author Wouter Beek
@version 2015/07-2015/08, 2015/12
*/

:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdfs/rdfs_read)).

:- rdf_meta(rdf_image(r,r,?)).
:- rdf_meta(rdf_image(r,r,?,r)).





%! rdf_image(?Subject:rdf_term, ?Predicate:iri, ?Image:atom) is nondet.
% Wrapper around rdf_image/4 with uninstantiated graph.

rdf_image(S, P, O):-
  rdf_image(S, P, O, _).


%! rdf_image(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Image:atom,
%!   ?Graph:rdf_graph
%! ) is nondet.

rdf_image(S, P, V, G):-
  (   rdf_member(P, [dbo:thumbnail,foaf:depiction]),
      rdf_literal(S, P, xsd:anyURI, V, G)
  ;   rdf_literal(S, P, xsd:anyURI, V, G),
      rdfs_instance(V, dcmit:'Image')
  ).
