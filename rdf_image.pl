:- module(
  rdf_image,
  [
    rdf_assert_image/5, % +Options:list(nvpair)
                        % +Subject:or([bnode,iri])
                        % +Predicate:iri
                        % +Object:or([bnode,iri,literal])
                        % +Graph:atom
    rdf_image/3, % +Options:list(nvpair)
                 % +URL:atom
                 % -File:atom
    rdf_image/5 % ?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Object:or([bnode,iri,literal])
                % ?ImageFile:atom
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
@version 2014/01
*/

:- use_module(generics(deb_ext)).
:- use_module(generics(uri_ext)).
:- use_module(http(http_download)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(rdf_assert_image(+,r,r,r,+)).
:- rdf_meta(rdf_image(r,r,r,-,?)).

:- xml_register_namespace(dcmit, 'http://purl.org/dc/dcmitype/').




rdf_assert_image(O1, S, P, O, G):-
  rdf_image(O1, O, _),
  is_image_url(O),
  rdf_assert_individual(O, dcmit:'Image', G),
  rdf_assert(S, P, O, G).


%! rdf_image(+Options:list(nvpair), +URL:atom, -File:atom) is det.
% Image storage on the Web is often unreliable,
% because Web locations go in and out of existence rapidly.
% We do not want automated procedures to fail due to unavailable images.
% Therefore the option `fail_mode` can be used with the following values:
%   1. `debug(Category:atom-Format:atom-Arguments:list)`
%   2. `error(Exception:compound)`
%   3. `fail`
%   4. `ignore`

rdf_image(O1, URL, File):-
  url_nested_file(data(.), URL, File),
  (
    access_file(File, exist), !
  ;
    download_to_file(O1, URL, File), !
  ;
    option(
      fail_mode(FM),
      O1,
      debug(rdf_image-'Could not fetch image from ~w'-[URL])
    ),
    fail_mode(FM)
  ).


rdf_image(S, P, O, File, G):-
  rdf(S, P, O, G),
  rdfs_individual_of(O, dcmit:'Image'),
  rdf_image([fail_mode(fail)], O, File).

