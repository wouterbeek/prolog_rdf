:- module(
  q_bnode_map,
  [
    q_bnode_map/2 % +B, -Name
  ]
).

/** <module> Quine blank node map

@author Wouter Beek
@version 2016/06
*/

:- dynamic
    q_bnode_map0/2.





%! q_bnode_map(+B, -Name) is det.

q_bnode_map(B, Name) :-
  q_bnode_map0(B, Name), !.
q_bnode_map(B, Name) :-
  flag(q_bnode_counter, Name0, Name0 + 1),
  Name is Name0 + 1,
  assert(q_bnode_map0(B, Name)).
