:- module(
  lodcache,
  [
    add_to_lod_pool/1, % +Resource:iri
    process_lod_pool/0
  ]
).

/** <module> LOD Caching

Generic predicates for caching Linked Open Data data.
This uses egograph caching and whitelisted RDF properties together with
datatype preferences in order to perform limited-scale crawling.

### Example

```prolog
?- add_to_lod_pool(dbpedia:'Monkey').
?- process_lod_pool.
```

--

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_debug)).
:- use_module(library(debug)).
:- use_module(library(lodcache/lodcache_egograph)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).

:- dynamic(in_lod_pool/1).
:- dynamic(lod_caching/1).

:- rdf_meta(add_to_lod_pool(r)).

%! lodcache:triple_to_iri(+Triples:compound, -Iri:atom) is nondet.

:- dynamic(lodcache:triple_to_iri/2).
:- multifile(lodcache:triple_to_iri/2).

lodcache:triple_to_iri(rdf(_,P,_), P).
lodcache:triple_to_iri(rdf(_,_,literal(type(D,_))), D).
lodcache:triple_to_iri(rdf(_,P,O), O):-
  rdf_memberchk(P, [owl:sameAs,rdf:type,rdfs:subClassOf,rdfs:subPropertyOf]).

:- initialization((
     assert_cc_prefixes,
     forall(between(1, 5, _), process_lod_pool)
   )).





%! add_to_lod_pool(+Iri:atom) is det.

add_to_lod_pool(Iri):-
  with_mutex(lod_pool, (
    (   % If a local graph with the RDF Name exists then we consider it to be
        % a safe enough bet that the RDF Name has been ego-cached.
        rdf_graph(Iri)
    ->  true
    ;   % ???
        in_lod_pool(Iri)
    ->  true
    ;   % The RDF Name is currently being cached,
        % so do not add it to the LOD Pool.
        lod_caching(Iri)
    ->  true
    ;   % Add the RDF Name to the LOD Pool.
        assert(in_lod_pool(Iri)),
        dcg_debug(lodcache(pool), added_to_pool(Iri))
    )
  )).



%! process_lod_pool is det.
% Start processing the LOD Pool.

process_lod_pool:-
  thread_create(process_lod_pool0, _, [detached(true)]).

process_lod_pool0:-
  % Pick a seed point from the pool:
  % Change the status of an RDF IRI to caching.
  with_mutex(lod_pool, (
    retract(in_lod_pool(Iri)),
    assert(lod_caching(Iri))
  )), !,
  
  % LOD Caching goes over HTTP, so this step takes a lot of time.
  lodcache_egograph(Iri, Ts),
  forall(
    member(rdf(Iri,P,O), Ts),
    rdf_assert(Iri, P, O, Iri)
  ),

  % Remove the caching status for the RDF IRI.
  with_mutex(lod_pool, retract(lod_caching(Iri))),
  
  % Extract new RDF IRIs to be seed points in the LOD Pool.
  % This effectively executes traversal over the LOD Graph.
  triples_to_visit_iris(Ts, Iris),
  maplist(add_to_lod_pool, Iris),
  
  dcg_debug(lodcache(pool), added_to_db(Iri, Ts)),
  process_lod_pool0.
process_lod_pool0:-
  % Pause for 5 seconds if there is nothing to process.
  sleep(5),
  process_lod_pool0.



%! lod_cache_iri(+Iri:iri, -Triples:ordset(compound)) is det.
% A special case occurs where a resource exists with empty dereference.
% An empty named graph indicates that this resource was processed.

lod_cache_iri(Iri, Ts):-
  % By creating the graph up front we prevent other threads
  % from caching the same resource.
  with_mutex(lod_pool, rdf_create_graph(Iri)),
  lodcache_egograph(Iri, Ts).



%! triples_to_visit_iris(+Triples:list(compound), -Iris:ordset(atom)) is det.

triples_to_visit_iris(Ts, Iris):-
  aggregate_all(
    set(Iri),
    (
      member(T, Ts),
      lodcache:triple_to_iri(T, Iri),
      % Filter out already cached IRIs at an early stage.
      \+ rdf_graph(Iri)
    ),
    Iris
  ).





% DEBUG %

added_to_db(Iri, Ts) -->
  "Asserted ",
  rdf_print_term(Iri),
  ": ",
  {length(Ts, N)},
  integer(N),
  " triples.".



added_to_pool(Iri) -->
  "Added ",
  rdf_print_term(Iri),
  " to pool of size ",
  {aggregate_all(count, in_lod_pool(_), N)},
  integer(N),
  ".".
