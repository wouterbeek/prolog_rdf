:- module(
  rdf_random,
  [
    rdf_random_neighbor/4, % +Vertex:rdf_term
                           % -RandomNeighborVertex:rdf_term
                           % +Graph:rdf_graph
                           % +Options:list(compound)
    rdf_random_term/2, % -RandomTerm:rdf_term
                       % +Graph:rdf_graph
    rdf_random_triple/2, % -RandomTriple:rdf_triple
                         % +Graph:rdf_graph
    rdf_random_triple/4 % ?RandomSubject:rdf_term
                        % ?RandomPredicate:iri
                        % ?RandomObject:rdf_term
                        % ?Graph:rdf_graph
  ]
).

/** <module> RDF random

@author Wouter Beek
@version 2013/09, 2014/02, 2014/06-2014/07, 2014/11, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(rdf/rdf_read)).

:- use_module(plc(generics/flag_ext)).

:- use_module(plRdf(graph/rdf_graph_theory)).

:- rdf_meta(rdf_index(?,r,r,o,?)).
:- rdf_meta(rdf_random_neighbor(+,r,r)).
:- rdf_meta(rdf_random_triple(r,r,o,?)).

:- predicate_options(rdf_random_neighbor/4, 4, [
     pass_to(rdf_neighbor_vertex/4, 4)
   ]).





%! rdf_index(
%!   ?Index:nonneg,
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:rdf_graph
%! ) is nondet.
% Returns the rdf triple that has the given index in the arbitrary sequence
% in which SWI-Prolog returns its triples.

rdf_index(I, S, P, O, G) :-
  state_init(State),
  repeat,
  (rdf(S, P, O, G) ; !),
  state_tick(State, I0),
  I0 == I, !.



%! rdf_random_neighbor(
%!   +Term:rdf_term,
%!   -RandomNeighbor:rdf_term,
%!   +Graph:rdf_graph,
%!   +Options:list(compound)
%! ) is det.
% Options are passed to rdf_neighbor_vertex/4.

rdf_random_neighbor(V, RndN, Graph, Opts) :-
  aggregate_all(
    set(N),
    rdf_neighbor_vertex(V, N, Graph, Opts),
    Ns
  ),
  random_member(RndN, Ns).



%! rdf_random_term(-Term:rdf_term, +Graph:rdf_graph) is det.

rdf_random_term(Term, Graph) :-
  rdf_random_triple(S, P, O, Graph),
  random_member(Term, [S,P,O]).



%! rdf_random_triple(-Triple:rdf_triple, +Graph:rdf_graph) is det.
% Wrapper around rdf_random_triple/4.

rdf_random_triple(rdf(S,P,O), G) :-
  rdf_random_triple(S, P, O, G).



%! rdf_random_triple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:rdf_graph
%! ) is det.
% Returns a random triple from the given graph.

rdf_random_triple(S, P, O, G) :-
  rdf_graph_get_property(G, triples(N)),
  random_between(1, N, RndI),
  rdf_index(RndI, S, P, O, G).
