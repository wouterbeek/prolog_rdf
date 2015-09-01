:- module(
  lod_cache,
  [
    add_to_lod_pool/1, % +Resource:iri
    lod_cache_iri/1, % +Iri:atom
    process_lod_pool/0,
    show_cache/0
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
:- use_module(library(deb_ext)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(owl/id_store)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(solution_sequences)).
:- use_module(library(uri)).

:- dynamic(in_lod_pool/1).
:- dynamic(lod_cached/1).
:- dynamic(lod_caching/1).

:- rdf_meta(add_to_lod_pool(r)).

%! lod_cache:triple_to_iri(+Triples:compound, -Iri:atom) is nondet.

:- dynamic(lod_cache:triple_to_iri/2).
:- multifile(lod_cache:triple_to_iri/2).

lod_cache:triple_to_iri(rdf(_,P,_), P).
lod_cache:triple_to_iri(rdf(_,_,literal(type(D,_))), D).
lod_cache:triple_to_iri(rdf(_,P,O), O):-
  rdf_memberchk(P, [owl:sameAs,rdf:type,rdfs:subClassOf,rdfs:subPropertyOf]),
  \+ rdf_is_literal(O),
  \+ rdf_is_bnode(O).

:- thread_local(count_triples/1).



show_cache:-
  aggregate_all(count, lod_cached(_), N2),
  aggregate_all(count, lod_caching(_), N3),
  aggregate_all(count, in_lod_pool(_), N1),
  format(user_output, 'LOD Pool~n  DONE:  ~D~n  DOING: ~D~n  TODO:  ~D~n', [N1,N2,N3]).

%! add_to_lod_pool(+Iri:atom) is det.

add_to_lod_pool(Iri):-
  with_mutex(lod_cache, (
    (   lod_cached(Iri)
    ->  true
    ;   in_lod_pool(Iri)
    ->  true
    ;   % The RDF Name is currently being cached,
        % so do not add it to the LOD Pool.
        lod_caching(Iri)
    ->  true
    ;   % Add the RDF Name to the LOD Pool.
        assert(in_lod_pool(Iri)),
        dcg_debug(lod_cache(pool), added_to_pool(Iri))
    )
  )).



%! lod_cache_iri(+Iri:iri) is det.

lod_cache_iri(Iri):-
  retractall(count_triples(_)), assert(count_triples(0)),
  rdf_load_triple(Iri, rdf_assert_triples0).

rdf_assert_triples0(Ts, _):-
  maplist(rdf_assert_triple0, Ts).

rdf_assert_triple0(rdf(S,P,O)):-
  catch((
    retract(count_triples(Old)), succ(Old, New), assert(count_triples(New)),
    forall(lod_cache:triple_to_iri(rdf(S,P,O), Iri), add_to_lod_pool(Iri)),
    rdf_assert3(S, P, O)
  ), E, format(user_error, '~q~n', [E])).



%! process_lod_pool is det.
% Start processing the LOD Pool.

process_lod_pool:-
  NumberOfThreads = 5,
  forall(between(1, NumberOfThreads, N), (
    format(atom(Alias), 'LOD Cache ~D', [N]),
    thread_create(process_lod_pool0, _, [alias(Alias),detached(true)])
  )).

% Process a seed point from the pool
process_lod_pool0:-
  % Change the status of an RDF IRI to caching.
  with_mutex(lod_cache, (
    retract(in_lod_pool(Iri)),
    assert(lod_caching(Iri))
  )), !,

  lod_cache_iri(Iri),
  retract(count_triples(N)),
  dcg_debug(lod_cache(pool), added_to_db(Iri, N)),

  % Remove the caching status for the RDF IRI.
  with_mutex(lod_cache, (
    retract(lod_caching(Iri)),
    assert(lod_cached(Iri))
  )),

  process_lod_pool0.
% Pause for 5 seconds if there is nothing to process.
process_lod_pool0:-
  sleep(5),
  process_lod_pool0.





% DEBUG %

added_to_db(Iri, N) -->
  "Asserted ",
  rdf_print_term(Iri),
  ": ",
  integer(N),
  " triples.".



added_to_pool(Iri) -->
  "Added ",
  rdf_print_term(Iri),
  " to pool of size ",
  {aggregate_all(count, in_lod_pool(_), N)},
  integer(N),
  ".".
