:- module(
  z_ext,
  [
    z_aggregate_all/3, % +Template, :Goal, -Result
    z_graph/1          % ?G
  ]
).

/** <module> Z extensions

Generic support for the Z abstraction layer.

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(hdt/hdt_ext)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    z_aggregate_all(+, 0, -).

:- rdf_meta
   z_aggregate_all(+, t, -),
   z_graph(r).





%! z_aggregate_all(+Template, :Goal, -Result) is det.

z_aggregate_all(Template, Goal, Result) :-
  aggregate_all(Template, Goal, Result).



%! z_graph(?G) is nondet.

z_graph(G) :-
  rdf_graph(G).
z_graph(G) :-
  hdt_graph(G).
