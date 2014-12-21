:- module(
  rdf_image,
  [
    rdf_assert_image/5, % +Options:list(nvpair)
                        % +Subject:or([bnode,iri])
                        % +Predicate:iri
                        % +Object:rdf_term
                        % +Graph:atom
    rdf_image/3, % ?Subject:or([bnode,iri])
                 % ?Predicate:iri
                 % ?Object:rdf_term
    rdf_image/4 % ?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Object:rdf_term
                % ?Graph:atom
  ]
).

/** <module> RDF image

Storing images in RDF is easy: just include a URL to the image.
But Web resources are not always sustainable,
 so images often go missing.

This module caches images when they are asserted in RDF.
When triples including images are read,
 the cache is updated if it has changed
 (and an image resource is available online).

@author Wouter Beek
@version 2014/01, 2014/12
*/

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(deb_ext)).
:- use_module(os(image_ext)).

:- use_module(plUri(image_uri)).
:- use_module(plUri(uri_ext)).

:- use_module(plHttp(download_to_file)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(management/rdf_prefix)).

:- rdf_meta(rdf_assert_image(+,r,r,r,+)).
:- rdf_meta(rdf_image(r,r,o)).
:- rdf_meta(rdf_image(r,r,o,?)).

:- rdf_register_prefix(dcmit, 'http://purl.org/dc/dcmitype/').





rdf_assert_image(O1, S, P, O, G):-
  rdf_image(O1, O, _),
  is_image_uri(O),
  rdf_assert_instance(O, dcmit:'Image', G),
  rdf_assert(S, P, O, G).



rdf_image(S, P, O):-
  rdf_image(S, P, O, _).



rdf_image(S, P, O, G):-
  rdf_member(P, [dbo:thumbnail,foaf:depiction]),
  rdf_reachable(S, owl:sameAs, S0),
  rdf(S0, P, O, G).
