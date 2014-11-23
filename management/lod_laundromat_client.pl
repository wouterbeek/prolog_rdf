:- module(
  lod_laundromat_client,
  [
    load_uri/1 % +Uri:atom
  ]
).

/** <module> LOD Laundromat: Client

Open an RDF dataset from the LOD Laundromat Web-service.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(http/http_open)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(zlib)).

load_uri(Uri):-
  setup_call_cleanup(
    http_open(Uri, In1, []),
    setup_call_cleanup(
      zopen(In1, In2, [close_parent(false)]),
      rdf_load(In2, [format(ntriples),graph(Uri),silent(true)]),
      close(In2)
    ),
    close(In1)
  ).
