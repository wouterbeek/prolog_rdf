:- module(
  rdf_random,
  [
    rdf_random_neighbor/4, % +Vertex:rdf_term
                           % -RandomNeighborVertex:rdf_term
                           % +Graph:atom
                           % +Options:list(nvpair)
    rdf_random_term/2, % -RandomTerm:rdf_term
                       % +Graph:atom
    rdf_random_triple/2, % -RandomTriple:compound
                         % +Graph:atom
    rdf_random_triple/4 % ?RandomSubject:or([bnode,iri])
                        % ?RandomPredicate:iri
                        % ?RandomObject:rdf_term
                        % ?Graph:atom
  ]
).

/** <module> RDF random

@author Wouter Beek
@version 2013/09, 2014/02, 2014/06-2014/07, 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(flag_ext)).

:- use_module(plRdf(graph/rdf_graph_theory)).

:- rdf_meta(rdf_index(?,r,r,o,?)).
:- rdf_meta(rdf_random_neighbor(+,r,r)).
:- rdf_meta(rdf_random_triple(r,r,o,?)).

:- predicate_options(rdf_random_neighbor/4, 4, [
     pass_to(rdf_neighbor_vertex/4, 4)
   ]).



%! rdf_index(
%!   ?Index:integer,
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
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
  (   rdf(S, P, O, Graph)
  ;   !
  ),
  state_tick(State, I0),
  I0 == I, !.



%! rdf_random_neighbor(
%!   +Term:rdf_term,
%!   -RandomNeighbor:rdf_term,
%!   +Graph:atom,
%!   +Options:list(nvpair)
%! ) is det.
% Options are passed to rdf_neighbor_vertex/4.

rdf_random_neighbor(V, RndN, Graph, Options):-
  aggregate_all(
    set(N),
    rdf_neighbor_vertex(V, N, Graph, Options),
    Ns
  ),
  random_member(RndN, Ns).



%! rdf_random_term(-Term:rdf_term, +Graph:atom) is det.

rdf_random_term(Term, Graph):-
  rdf_random_triple(S, P, O, Graph),
  random_member(Term, [S,P,O]).



%! rdf_random_triple(-Triple:compound, +Graph:graph) is det.
% Wrapper around rdf_random_triple/4.

rdf_random_triple(rdf(S,P,O), G):-
  rdf_random_triple(S, P, O, G).



%! rdf_random_triple(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
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

