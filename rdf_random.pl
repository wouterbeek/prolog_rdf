:- module(
  rdf_random,
  [
    rdf_random_neighbor/4, % +Graph:atom
                           % +Vertex:or([bnode,literal,iri])
                           % -RandomNeighborVertex:or([bnode,literal,iri])
                           % +Options:list(nvpair)
    rdf_random_term/2, % +Graph:atom
                       % -Term:or([bnode,literal,iri])
    rdf_random_term/3, % +Graph:atom
                       % :Requirement
                       % -Term:or([bnode,literal,iri])
    rdf_random_triple/2, % -RandomTriple:compound
                         % +Graph:atom
    rdf_random_triple/4 % ?RandomSubject:or([bnode,iri])
                        % ?RandomPredicate:iri
                        % ?RandomObject:or([bnode,iri,literal])
                        % ?Graph:atom
  ]
).

/** <module> RDF random

@author Wouter Beek
@version 2013/09, 2014/02, 2014/06-2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(predicate_options)). % Declarations.
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_generic)).
:- use_module(generics(flag_ext)).

:- use_module(plRdf(rdf_graph_theory)).
:- use_module(plRdf(rdf_parse)).

:- meta_predicate(rdf_random_term(+,//,-)).

:- rdf_meta(rdf_index(?,r,r,o,?)).
:- rdf_meta(rdf_random_neighbor(+,r,r)).
:- rdf_meta(rdf_random_term(+,r)).
:- rdf_meta(rdf_random_term(+,:,r)).
:- rdf_meta(rdf_random_triple(r,r,o,?)).

:- predicate_option(rdf_random_neighbor/4, 4, [
     pass_to(rdf_neighbor_vertex/4, 4)
   ]).



%! rdf_index(
%!   ?Index:integer,
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?Graph:graph
%! ) is nondet.
% Returns the rdf triple that has the given index in the arbitrary sequence
% in which SWI-Prolog returns its triples.
%
% @arg Index A compound term of the form =Graph:Line= with =Graph= the
%      atomic name of an RDF graph and =Line= an integer.
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Object A resource.
% @arg Graph The atomic name of a graph.

rdf_index(I, S, P, O, Graph):-
  state_init(State),
  repeat,
  (
    rdf(S, P, O, Graph)
  ;
    !
  ),
  state_tick(State, I0),
  I0 == I, !.


%! rdf_random_neighbor(
%!   +Graph:atom,
%!   +Term:or([bnode,iri,literal]),
%!   -RandomNeighbor:or([bnode,iri,literal]),
%!   +Options:list(nvpair)
%! ) is det.
% Options are passed to rdf_neighbor_vertex/4.

rdf_random_neighbor(Graph, V, RndN, Options):-
  aggregate_all(
    set(N),
    rdf_neighbor_vertex(Graph, V, N, Options),
    Ns
  ),
  random_member(RdN, Ns).


rdf_random_term(G, T):-
  rdf_random_term(G, rdf_parse_term(_), T).

rdf_random_term(G, Requirement, T2):-
  rdf_random_triple(S, P, O, G),
  random_between(1, 2, J),
  nth1(J, [S,P,O], T1),
  (
    dcg_phrase(Requirement, T1)
  ->
    T2 = T1
  ;
    rdf_random_term(G, Requirement, T2)
  ).


%! rdf_random_triple(-Triple:compound, +Graph:graph) is det.
% Wrapper around rdf_random_triple/4.

rdf_random_triple(rdf(S,P,O), G):-
  rdf_random_triple(S, P, O, G).

%! rdf_random_triple(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?Graph:graph
%! ) is det.
% Returns a random triple from the given graph.
%
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Object A resource.
% @arg Graph The atomic name of a graph.

rdf_random_triple(S, P, O, Graph):-
  rdf_graph_property(Graph, triples(NumberOfTriples)),
  random_between(1, NumberOfTriples, RndI),
  rdf_index(RndI, S, P, O, Graph).

