:- module(
  rdf_random,
  [
    rdf_random_neighbor/3, % +Graph:atom
                           % +Vertex:or([bnode,literal,iri])
                           % -RandomNeighbor:or([bnode,literal,iri])
    rdf_random_term/2, % +Graph:atom
                       % -Term:or([bnode,literal,iri])
    rdf_random_term/3, % +Graph:atom
                       % :Requirement
                       % -Term:or([bnode,literal,iri])
    rdf_random_triple/4 % +Graph:atom
                        % -RandomSubject:or([bnode,iri])
                        % -RandomPredicate:iri
                        % -RandomObject:or([bnode,iri,literal])
  ]
).

/** <module> RDF_RANDOM

@author Wouter Beek
@version 2013/09, 2014/02
*/

:- use_module(dcg(dcg_generic)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_parse)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf_graph(rdf_graph_theory)).

:- rdf_meta(rdf_index(r,r,r,?,?)).
:- rdf_meta(rdf_random_neighbor(+,r,r)).
:- rdf_meta(rdf_random_term(+,r)).
:- rdf_meta(rdf_random_term(+,:,r)).
:- rdf_meta(rdf_random_triple(+,r,r,r)).



%! rdf_index(
%!   ?Graph:graph,
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?Index:integer
%! ) is nondet.
% Returns the rdf triple that has the given index in the arbitrary sequence
% in which SWI-Prolog returns its triples.
%
% @arg Graph The atomic name of a graph.
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Object A resource.
% @arg Index A compound term of the form =Graph:Line= with =Graph= the
%        atomic name of an RDF graph and =Line= an integer.

rdf_index(G, S, P, O, I):-
  rdf_graph:rdf_graph_to_triples(G, Triples),
  nth0(I, Triples, rdf(S, P, O)).

rdf_random_neighbor(G, V, RndN):-
  rdf_neighbors(G, V, Ns),
  length(Ns, L),
  random_between(1, L, I),
  nth1(I, Ns, RndN).

rdf_random_term(G, T):-
  rdf_random_term(G, rdf_parse_term(_), T).

:- meta_predicate(rdf_random_term(+,//,-)).
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

%! rdf_random_triple(
%!   ?Graph:graph,
%!   -Subject:oneof([bnode,iri]),
%!   -Predicate:iri,
%!   -Object:or([bnode,literal,iri])
%! ) is det.
% Returns a random triple from the given graph.
%
% @arg Graph The atomic name of a graph.
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Object A resource.

rdf_random_triple(G, S, P, O):-
  rdf_graph_property(G, triples(NumberOfTriples)),
  random_between(1, NumberOfTriples, RndI),
  rdf_index(G, S, P, O, RndI).

