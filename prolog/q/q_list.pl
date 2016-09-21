:- module(
  q_list,
  [
    q_last/4,         % ?M, ?L,  ?O,  ?G
    q_last/5,         % ?M, ?S,  ?P,  ?O, ?G
    q_list/3,         % ?M, ?L,  ?G
    q_list/4,         % ?M, ?L1, -L2, ?G
    q_list/5,         % ?M, ?S,  ?P,  ?L, ?G
    q_list/6,         % ?M, ?S,  ?P,  ?L1, -L2, ?G
    q_list_member/4,  % ?M, ?L,  ?O,  ?G
    q_list_member/5,  % ?M, ?S,  ?P,  ?O, ?G
    q_maximal_list/3, % ?M, ?L,  ?G
    q_maximal_list/4, % ?M, ?L1, ?L2, ?G
    q_maximal_list/5, % ?M, ?S,  ?P,  ?L, ?G
    q_maximal_list/6, % ?M, ?S,  ?P,  ?L1, ?L2, ?G
    qb_list/4,        % +M, +L1, -L2, +G
    qb_list/5         % +M, +S,  +P,  +L, +G
  ]
).

/** <module> Quine list API

@author Wouter Beek
@version 2016/06-2016/07
*/

:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   q_last(?, r, o, r),
   q_last(?, r, r, o, r),
   q_list(?, r, r),
   q_list(?, r, -, r),
   q_list(?, r, r, r, r),
   q_list(?, r, r, r, -, r),
   q_list_member(?, r, o, r),
   q_list_member(?, r, r, o, r),
   q_maximal_list(?, r, r),
   q_maximal_list(?, r, -, r),
   q_maximal_list(?, r, r, r, r),
   q_maximal_list(?, r, r, r, -, r),
   qb_list(+, t, -, r),
   qb_list(+, r, r, t, r).





%! q_last(?M, ?L, ?O, ?G) is nondet.
%! q_last(?M, ?S, ?P, ?O, ?G) is nondet.
%
% O is the last item in an RDF list.

q_last(M, L, O, G) :-
  q(M, L, rdf:rest, T, G),
  (   rdf_equal(T, rdf:nil)
  ->  q(M, L, rdf:first, O, G)
  ;   q_last(M, T, O, G)
  ).


q_last(M, S, P, O, G) :-
  q_list(M, S, P, L, G),
  q_last(M, L, O, G).



%! q_list(?M, ?L, ?G) is nondet.
%! q_list(?M, ?L1, -L2, ?G) is nondet.
%! q_list(?M, ?S, ?P, ?L, ?G) is nondet.
%! q_list(?M, ?S, ?P, ?L1, -L2, ?G) is nondet.
%
% Enumerates lists L that are not (strict) sublists.

q_list(M, L1, G) :-
  q_list_first(M, L1, L2, G),
  q_list_rest(M, L2, G).


q_list(M, L1, [H|T], G) :-
  q_list_first(M, L1, H, L2, G),
  q_list_rest(M, L2, T, G).


q_list(M, S, P, L, G) :-
  q(M, S, P, L, G),
  q_list(M, L, G).


q_list(M, S, P, L1, L2, G) :-
  q(M, S, P, L1, G),
  q_list(M, L1, L2, G).


q_maximal_list(M, L1, G) :-
  q_list_first(M, L1, L2, G),
  \+ q(M, _, rdf:rest, L1, G),
  q_list_rest(M, L2, G).


q_maximal_list(M, L1, [H|T], G) :-
  q_list_first(M, L1, H, L2, G),
  \+ q(M, _, rdf:rest, L1, G),
  q_list_rest(M, L2, T, G).


q_maximal_list(M, S, P, L, G) :-
  q(M, S, P, L, G),
  q_maximal_list(M, L, G).


q_maximal_list(M, S, P, L1, L2, G) :-
  q(M, S, P, L1, G),
  q_maximal_list(M, L1, L2, G).


q_list_first(M, L1, L2, G) :-
  once(q(M, L1, rdf:first, _, G)),
  q(M, L1, rdf:rest, L2, G).


q_list_first(M, L1, H, L2, G) :-
  q(M, L1, rdf:first, H, G),
  q(M, L1, rdf:rest, L2, G).


q_list_rest(M, L1, G) :-
  q_list_first(M, L1, L2, G),
  q_list_rest(M, L2, G).
q_list_rest(_, rdf:nil, _).


q_list_rest(M, L1, [H|T], G) :-
  q_list_first(M, L1, H, L2, G),
  q_list_rest(M, L2, T, G).
q_list_rest(_, rdf:nil, [], _).



%! q_list_member(?M, ?L, ?O, ?G) is nondet.
%! q_list_member(?M, ?S, ?P, ?O, ?G) is nondet.

q_list_member(M, L, O, G) :-
  q(M, L, rdf:first, O, G).
q_list_member(M, L, O, G) :-
  q(M, L, rdf:rest, T, G),
  q_list_member(M, T, O, G).



q_list_member(M, S, P, O, G) :-
  ground(O), !,
  q_list_member(M, L, O, G),
  q(M, S, P, L, G).
q_list_member(M, S, P, O, G) :-
  q(M, S, P, L, G),
  q_list_member(M, L, O, G).



%! qb_list(+M, +L1, -L2, +G) is det.
%! qb_list(+M, +S, +P, +L, +G) is det.

qb_list(M, L1, L2, G) :-
  rdf_transaction(qb_list0(M, L1, L2, G)).


qb_list0(_, [], L, _) :-
  rdf_equal(rdf:nil, L).
qb_list0(M, [H|T], L, G) :-
  (var(L) -> qb_bnode(L) ; true),
  % @tbd RDF alias expansion does not work.
  rdf_equal(rdf:'List', C),
  qb(M, L, rdf:type, C, G),
  qb(M, L, rdf:first, H, G),
  (   T == []
  ->  % @tbd RDF alias expansion does not work.
      rdf_equal(rdf:nil, I),
      qb(M, L, rdf:rest, I, G)
  ;   qb_bnode(T2),
      qb(M, L, rdf:rest, T2, G),
      qb_list0(M, T, T2, G)
  ).


qb_list(M, S, P, L1, G) :-
  rdf_transaction((
    qb_list0(M, L1, L2, G),
    qb(M, S, P, L2, G)
  )).
