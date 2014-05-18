:- module(
  rdf_search,
  [
    rdf_beam/5, % +Options:list(nvpair)
                % +RootVertex:vertex
                % +Predicates:list
                % -Vertices:ordset(vertex)
                % -Edges:ordset(edge)
    rdf_breadth_first/5 % +Set:oneof([ground,list,ordset])
                        % +R1:iri
                        % +R2:iri
                        % -Sol1:ordset
                        % -Sol2:ordset
  ]
).

/** <module> RDF SEARCH

Searching through an RDF graph.

@author Wouter Beek
@version 2013/05
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_graph(rdf_graph_theory)).

:- rdf_meta(rdf_beam(+,r,+,-,-)).
:- rdf_meta(rdf_breadth_first(+,r,r,-,-)).
:- rdf_meta(rdf_breadth_first(+,r,r,+,+,-,-)).



%! rdf_beam(
%!   +Options:list(nvpair),
%!   +RootVertex,
%!   +Predicates:list,
%!   -Vertices:ordset(vertex),
%!   -Edges:ordset(edge)
%! ) is det.

rdf_beam(O, V, Ps, Vs, Es):-
  rdf_beam(O, [V], Ps, Vs, [], Es).

rdf_beam(_O, [], _Ps, AllVs, AllEs, AllEs):-
  rdf_edges_to_vertices(AllEs, AllVs), !.
rdf_beam(O, Vs, Ps, AllVs, Es, AllEs):-
  aggregate_all(
    set(V-NextV),
    (
      member(V, Vs),
      member(P, Ps),
      rdf_has(V, P, NextV),
      \+ member(V-NextV, Es)
    ),
    NextEs
  ),
  ord_union(Es, NextEs, NewEs),
  rdf_edges_to_vertices(NextEs, NextVs),
  rdf_beam(O, NextVs, Ps, AllVs, NewEs, AllEs).

%! rdf_breadth_first(
%!   +As:or([iri,list(iri),ordset(iri)]),
%!   +Relation_AB:iri,
%!   +Relation_BA:iri,
%!   -As:ordset(iri),
%!   -Bs:ordset(iri)
%! ) is det.

% Make sure we start out with an ordered set.
rdf_breadth_first(Set, R1, R2, Sol1, Sol2):-
  is_ordset(Set), !,
  rdf_breadth_first(Set, R1, R2, Set, [], Sol1, Sol2).
% Lists are converted to ordered sets.
rdf_breadth_first(List, R1, R2, Sol1, Sol2):-
  is_list(List), !,
  list_to_ord_set(List, Set),
  rdf_breadth_first(Set, R1, R2, Sol1, Sol2).
% Single elements are placed inside a list.
rdf_breadth_first(Element, R1, R2, Sol1, Sol2):-
  ground(Element), !,
  rdf_breadth_first([Element], R1, R2, Sol1, Sol2).

rdf_breadth_first([], _R_AB, _R_BA, SolA, SolB, SolA, SolB):- !.
rdf_breadth_first(A1, R_AB, R_BA, HistA1, HistB1, SolA, SolB):-
  % Find all Bs that can be reached from some A using the former relation.
  aggregate_all(
    set(B),
    (
      member(A, A1),
      rdf_has(B, R_AB, A),
      \+ member(B, HistB1)
    ),
    B2
  ),
  % Find all As that can be reached from some B using the latter relation.
  aggregate_all(
    set(A),
    (
      member(B, B2),
      rdf_has(B, R_BA, A),
      \+ member(A, HistA1)
    ),
    A2
  ),
  
  % Update the histories, so we will not visit the same resource twice.
  ord_union(HistA1, A2, HistA2),
  ord_union(HistB1, B2, HistB2),
  
  rdf_breadth_first(A2, R_AB, R_BA, HistA2, HistB2, SolA, SolB).

