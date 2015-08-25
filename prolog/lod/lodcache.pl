:- module(
  lodcache,
  [
    add_to_lod_pool/1, % +Resource:iri
    load_as_egographs/1, % +Spec
    load_as_egographs/2, % +Spec:compound
                         % +Options:list(compound)
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
:- use_module(library(deb_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
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
:- dynamic(lod_caching/1).

:- rdf_meta(add_to_lod_pool(r)).

%! lodcache:triple_to_iri(+Triples:compound, -Iri:atom) is nondet.

:- dynamic(lodcache:triple_to_iri/2).
:- multifile(lodcache:triple_to_iri/2).

lodcache:triple_to_iri(rdf(_,P,_), P).
lodcache:triple_to_iri(rdf(_,_,literal(type(D,_))), D).
lodcache:triple_to_iri(rdf(_,P,O), O):-
  rdf_memberchk(P, [owl:sameAs,rdf:type,rdfs:subClassOf,rdfs:subPropertyOf]).

:- predicate_options(load_as_egographs/2, 2, [
     pass_to(rdf_load_any/2)
   ]).

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



%! load_as_egographs(+Spec:compound) is det.
% Wrapper around load_as_egographs/2 with default options.

load_as_egographs(Spec):-
  load_as_egographs(Spec, [select(true)]).

%! load_as_egographs(+Spec:compound, +Options:list(compound)) is det.
% Cache the contents of the given RDF file in terms of egographs.
%
% Options are passed to rdf_load_any/2.

load_as_egographs(Spec0, Opts0):-
  ground(Spec0),

  % Support for loading data based on a registered RDF prefix.
  (   Spec0 = prefix(Prefix)
  ->  rdf_current_prefix(Prefix, Spec)
  ;   Spec = Spec0
  ),

  % Thread-specific RDF graph name.
  thread_self(Id),
  atomic_list_concat([tmp,Id], '_', G),
  merge_options([graph(G)], Opts0, Opts),

  setup_call_cleanup(
    rdf_load_any(Spec, Opts),
    forall(
      distinct(S, rdf2(S, _, _)),
      rdf_mv(G, S, _, _, S)
    ),
    rdf_unload_graph(G)
  ).



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
  cache_egograph(Iri, Ts),
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



%! cache_egograph(+Iri:iri, -Triples:ordset(compound)) is det.
% Retrieves the triples that encompass the ego-graph of the given RDF IRI.
%
% ### Definition
%
% An **ego-graph** is a graph with one vertex in the middle
% (the aforementioned resource) and an arbitrary number of vertices
% surrounding it.
% The complete ego-graph of a resource gives the depth-1 description
% of that resource.
%
% The complete ego-graph is stored in an RDF graph with name `Resource`.
%
% @see http://faculty.ucr.edu/~hanneman/nettext/C9_Ego_networks.html

cache_egograph(Iri, Ts):-
  % By creating the graph up front we prevent other threads
  % from caching the same resource.
  with_mutex(lod_pool, rdf_create_graph(Iri)),

  % Temporary graph name.
  thread_self(Id),
  atomic_list_concat([Iri,Id], '_', G),

  catch(
    setup_call_cleanup(
      rdf_load_any(Iri, [graph(G)]),
      aggregate_all(set(rdf(Iri,P,O)), rdf2(Iri,P,O,G), Ts),
      rdf_unload_graph(G)
    ),
    E,
    (   E = exception(Err)
    ->  print_message(error, Err)
    ;   rdf_unload_graph(G)
    )
  ),

  if_debug(lodcache, (
    length(Ts, N),
    debug(lodcache, 'Loaded ~D triples for IRI ~a', [N,Iri])
  )).



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
