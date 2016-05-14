:- module(
  flickrwrappr,
  [
    is_flickrwrappr_iri/1, % +Iri
    flickrwrappr_cache/1,  % +G
    flickrwrappr_cache/3   % +G, -Resources, -Triples
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
@version 2014/01, 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).
:- use_module(library(yall)).

:- rdf_register_prefix(dbp, 'http://dbpedia.org/property/').





is_flickrwrappr_iri(Iri):-
  uri_components(Iri,  uri_components(_,'wifo5-03.informatik.uni-mannheim.de',Path,_,_)),
  file_directory_name(Path, Dir),
  Dir == '/flickrwrappr/photos'.



%! flickrwrappr_cache(+G) is det.
%! flickrwrappr_cache(+G, -Resources, -Triples) is det.

flickrwrappr_cache(G):-
  flickrwrappr_cache(G, _, Triples),
  maplist({G}/[Triple]>>assert_proposition(Triple, G), Triples).


flickrwrappr_cache(G, Resources, Triples):-
  aggregate_all(
    set(IRI-IRI),
    rdf(IRI, dbp:hasPhotoCollection, IRI, G),
    Pairs
  ),
  maplist(flickrwrappr_cache_url, Pairs, Resourcess, Tripless),
  maplist(ord_union, [Resourcess,Tripless], [Resources,Triples]).

flickrwrappr_cache_url(IRI1-IRI, Resources, Triples):-
  uri_query_add_nvpair(IRI1, format, rdf, IRI2),
  lod_local_query([], IRI2, _NoGraph, IRI, Resources, Triples).

assert_proposition(Graph, [S,P,O]):-
  rdf_assert(S, P, O, Graph).

