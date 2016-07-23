:- module(
  flickrwrappr,
  [
    is_flickrwrappr_iri/1, % +Iri
    flickrwrappr/2         % +S, -Quad
  ]
).

/** <module> FlickrWrappr

In DBpedia, FlickWrappr resources are linked to DBpedia resources in
triples like [1].  The RDF file retrieved at [2] describes resource
[3].

```
[1]   http://dbpedia.org/resource/Monkey dbpprop:hasPhotoCollection http://wifo5-03.informatik.uni-mannheim.de/flickrwrappr/photos/Monkey .
[2]   http://wifo5-03.informatik.uni-mannheim.de/flickrwrappr/photos/Monkey
[3]   http://dbpedia.org/resource/Monkey
```

Most of the time the LOD description at a IRI describes that IRI.  But
sometimes the LOD description at an IRI describes the previous
resource.  For example, the LOD description at location [2] describes
resource [3].  While caching, location [2] is reached via triple [1]
and the LOD description at location [2] makes assertions about
resource [3].

@author Wouter Beek
@see http://wifo5-03.informatik.uni-mannheim.de/flickrwrappr/
@version 2016/05
*/

:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- qb_alias(dbp, 'http://dbpedia.org/property/').

:- rdf_meta
   flickrwrappr(r, -).





is_flickrwrappr_iri(Iri):-
  uri_components(Iri,  uri_components(_,'wifo5-03.informatik.uni-mannheim.de',Path,_,_)),
  file_directory_name(Path, Dir),
  Dir == '/flickrwrappr/photos'.



%! flickrwrappr(+S, -Quad) is nondet.

flickrwrappr(S, Quad):-
  % NONDET
  rdf_has(S, dbp:hasPhotoCollection, Location1),
  iri_add_query_comp(Location1, format=rdf, Location2),
  rdf_load_quads(Location2, Quads),
  member(Quad, Quads).
