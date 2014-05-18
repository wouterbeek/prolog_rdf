:- module(
  rdf_graph_closure,
  [
    rdf_graph_closure/4 % +Resources1:ordset(or([bnode,iri,literal]))
                        % :Predicate
                        % -Resources2:ordset(or([bnode,iri,literal]))
                        % -Propositions:ordset(list)
  ]
).

/** <module> RDF Graph Closure

@author Wouter Beek
@version 2014/01
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- meta_predicate(graph_closure(+,3,-,-)).



graph_closure([], _, [], []):- !.
graph_closure([H|T], Pred, VSol, PropsSol):-
  graph_closure([H|T], Pred, [H], VSol, [], PropsSol).

graph_closure([H1|T1], Pred, Vs1, VSol, Props1, PropsSol):-
  call(Pred, H1, Neighbors, NewProps),
  exclude(in_ordset(Vs1), Neighbors, NewNeighbors),
  append(T1, NewNeighbors, T2),
  ord_union(Vs1, Neighbors, Vs2),
  ord_union(Props1, NewProps, Props2),
  graph_closure(T2, Pred, Vs2, VSol, Props2, PropsSol).

in_ordset(Set, Element):-
  memberchk(Element, Set).
