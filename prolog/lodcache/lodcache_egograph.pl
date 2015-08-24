:- module(
  lodcache_egograph,
  [
    lodcache_egograph/2 % +Iri:iri
                        % -Triples:ordset(compound)
  ]
).

/** <module> LOD-Cache ego-graphs

Collects triples that are one step removed from a given resource.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(solution_sequences)).
:- use_module(library(sparkle)).
:- use_module(library(sparql/sparql_db)).
:- use_module(library(uri)).

:- rdf_meta(lodcache_egograph(r,-)).





%! lodcache_egograph(+Iri:iri, -Triples:ordset(compound)) is det.
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

lodcache_egograph(Iri, Ts):-
  % There are three modes of assembling a LOD egograph:
  % (1) datadump, (2) dereference, (3) SPARQL.
  findall(Mode-Ts, lodcache_egograph_mode(Mode, Iri, Ts), Pairs),
  pairs_keys_values(Pairs, Modes, Tss),
  ord_union(Tss, Ts),

  % DEB
  (   debugging(lodcache(egograph))
  ->  maplist(length, [Ts|Tss], Ns),
      pairs_keys_values(Pairs, [all|Modes], Ns),
      print_message(informatonal, lodcache_egograph_debug(Pairs))
  ;   true
  ).



%! lodcache_egograph(
%!   +Iri:iri,
%!   -Terms:ordset(rdf_term),
%!   -Triples:ordset(compound)
%! ) is det.
% Returns the depth-one resources and propositions for the given RDF IRI.

lodcache_egograph(Iri, Terms, Ts):-
  lodcache_egograph(Iri, Ts),
  rdf_triples_to_terms(Ts, Terms).



%! lodcache_egograph_download(
%!   +Iri:iri,
%!   +Location:iri,
%!   -Triples:ordset(compound)
%! ) is det.

lodcache_egograph_download(Iri, Location, Ts):-
  % Different threads may download the same RDF IRI
  % (there is no coordinating communication/locking to prevent this).
  thread_self(Id),
  atomic_list_concat([Iri,Id], '_', G),

  catch(
    (
      setup_call_cleanup(
        rdf_load(Location, [graph(G)]),
        aggregate_all(set(rdf(Iri,P,O)), rdf2(Iri,P,O,G), Ts),
        rdf_unload_graph(G)
      )
    ),
    E,
    (   E = exception(Err)
    ->  print_message(error, Err)
    ;   rdf_unload_graph(G)
    )
  ).



%! lodcache_egograph_mode(
%!   +Mode:oneof([datadump,dereference,sparql]),
%!   +Iri:iri,
%!   -Triples:ordset(compound)
%! ) is det.

% **SPARQL query**
% SPARQL cache also covers the case for blank nodes and literals.
lodcache_egograph_mode(sparql, Iri, Ts):-
  once(sparql_endpoint_by_iri(Iri, Endpoint)),
  findall(rdf(S,P,O), query_goal(Endpoint, rdf(S, P, O), []), Ts).

% **Semi-dereference**
% There is a known location where resources denoted by IRIs
% with set prefix are described.
% These locations are registered using module lod_db.pl.
lodcache_egograph_mode(datatump, Iri, Ts):-
  rdf_global_id(Prefix:_, Iri),
  lodcache_egograph_download(Iri, Prefix, Ts).

% **Dereference**
lodcache_egograph_mode(dereference, Iri, Ts):-
  uri_iri(Location, Iri),
  lodcache_egograph_download(Iri, Location, Ts).



/*
%! lodcache_load_file_as_egographs(+Spec:compound) is det.
% Cache the contents of the given RDF file in terms of egographs.

lodcache_load_file_as_egographs(Spec):-
  thread_self(Id),
  atomic_list_concat([tmp,Id], '_', G),

  setup_call_cleanup(
    rdf_load_any(Spec, [graph(G)]),
    forall(
      distinct(S, rdf2(S, _, _)),
      rdf_mv(G, S, _, _, S)
    ),
    rdf_unload_graph(G)
  ).
*/





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(lodcache_egograph_debug([])) --> !,
  [nl].
prolog:message(lodcache_egograph_debug([Mode-Length|Pairs])) -->
  ['~a(~D) '-[Mode,Length]],
  prolog:message(lodcache_egograph_debug(Pairs)).
