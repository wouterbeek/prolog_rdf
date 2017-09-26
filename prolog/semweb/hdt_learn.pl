:- module(
  hdt_learn,
  [
    hdt_node_degree/3, % +Hdt, +Node, ?Degree
    hdt_step_random/4, % +Hdt, +V, -E, -W
    hdt_walk_random/4  % +Hdt, +Node, +Depth, -Walk
  ]
).
:- reexport(library(hdt_term)).

/** <module> HDT as a Machine Learning bakcend

@author Wouter Beek
@version 2017/09
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_print)).

:- rdf_meta
   hdt_node_degree(+, r, ?),
   hdt_step_random(+, r, -, -),
   hdt_walk_random(+, r, +, -).





%! hdt_node_degree(+Hdt:blob, +Node, ?Degree:nonneg) is nondet.

hdt_node_degree(Hdt, Node, Degree) :-
  hdt_triple_count(Hdt, Node, _, _, OutDegree),
  hdt_triple_count(Hdt, _, _, Node, InDegree),
  sum_list([OutDegree,InDegree], Degree).



%! hdt_step_random(+Hdt:blob, +V, -E, -W) is det.

hdt_step_random(Hdt, V, E, W) :-
  hdt_triple_count(Hdt, V, _, _, OutDegree),
  hdt_triple_count(Hdt, _, _, V, InDegree),
  sum_list([OutDegree,InDegree], Degree),
  random_between(1, Degree, Random),
  (   Random =< OutDegree
  ->  % forward link
      hdt_triple_random(Hdt, V, E, W),
      (   debugging(hdt_learn)
      ->  flag(number_of_steps, N, N+1),
          dcg_debug(hdt_learn, (
            thousands(N),
            " ⮞ ",
            rdf_dcg_iri(E),
            " ",
            rdf_dcg_term(W)
          ))
      ;   true
      )
  ;   % backward link
      hdt_triple_random(Hdt, W, E, V),
      (   debugging(hdt_learn)
      ->  flag(number_of_steps, N, N+1),
          dcg_debug(hdt_learn, (
            thousands(N),
            " ⮜ ",
            rdf_dcg_term(W),
            " ",
            rdf_dcg_iri(E)
          ))
      ;   true
      )
  ).



%! hdt_walk_random(+Hdt:blob, +Node, +Depth:nonneg, -Walk:list) is det.
%
% Walk is unified with a randomly chosen alternating sequence of
% vertices and edges, not necessarily starting and ending at the same
% vertex (/open/ walk).

hdt_walk_random(Hdt, V, D, Walk) :-
  must_be(nonneg, D),
  (debugging(hdt_learn) -> flag(number_of_steps, _, 1) ; true),
  hdt_walk_random_(Hdt, V, D, Walk).

hdt_walk_random_(_, V, 0, [V]) :- !.
hdt_walk_random_(Hdt, V, D1, [V,E|Walk]) :-
  hdt_step_random(Hdt, V, E, W),
  D2 is D1 - 1,
  hdt_walk_random_(Hdt, W, D2, Walk).
