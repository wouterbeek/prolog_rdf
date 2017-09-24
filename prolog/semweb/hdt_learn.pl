:- module(
  hdt_learn,
  [
    hdt_rnd_step/4, % +Node1, -EdgeLabel, -Node2, +Hdt
    hdt_rnd_walk/4  % +Node, +Depth, -Walk, +Hdt
  ]
).
:- reexport(library(hdt)).

/** <module> HDT as a Machine Learning bakcend

@author Wouter Beek
@version 2017/09
*/

:- use_module(library(random)).
:- use_module(library(semweb/rdf11)).





%! hdt_rnd_step(+Node1:rdf_term, -EdgeLabel:rdf_iri, -Node2:rdf_term,
%!              +Hdt:blob) is det.

hdt_rnd_step(Node1, P, Node2, Hdt) :-
  rdf_is_literal(Node1), !,
  hdt_rnd(Node2, P, Node1, Hdt).
hdt_rnd_step(Node1, P, Node2, Hdt) :-
  hdt_count(Node1, P, _, A, Hdt),
  (A =:= 0 -> true ; hdt_rnd(Node1, P, Node2A, Hdt)),
  hdt_count(_, P, Node1, B, Hdt),
  (B =:= 0 -> true ; hdt_rnd(Node2B, P, Node1, Hdt)),
  AB is A + B,
  random_between(1, AB, Offset),
  (Offset =< A -> Node2 = Node2A ; Node2 = Node2B).



%! hdt_rnd_walk(+Node:rdf_node, +Depth:nonneg, -Walk:list(rdf_term),
%!              +Hdt:blob) is det.
%
% Walk is unified with a randomly chosen alternating sequence of
% vertices and edges, not necessarily starting and ending at the same
% vertex (/open/ walk).

hdt_rnd_walk(Node, Depth, Walk, Hdt) :-
  must_be(nonneg, Depth),
  hdt_rnd_walk_(Node, Depth, Walk, Hdt).


hdt_rnd_walk_(Node, 0, [Node], _) :- !.
hdt_rnd_walk_(Node1, Depth1, [Node1,EdgeLabel|Walk], Hdt) :-
  hdt_rnd_step(Node1, EdgeLabel, Node2, Hdt),
  Depth2 is Depth1 - 1,
  hdt_rnd_walk(Node2, Depth2, Walk, Hdt).
