:- module(
  hdt_update,
  [
    hdt_update_tree/2 % +G, :Goal_1
  ]
).

/** <module> HDT update

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(apply)).
:- use_module(library(hdt/hdt__io)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_term)).
:- use_module(library(uuid_ext)).
:- use_module(library(yall)).

:- meta_predicate
    hdt_update_tree(+, 1).

:- rdf_meta
   hdt_update_tree(r, :).





%! hdt_update_tree(+G, :Goal_1) is det.

hdt_update_tree(G1, Goal_1) :-
  q_tree(hdt, Root, G1, Tree),
  setup_call_cleanup(
    (
      uuid_no_hyphen(G2),
      maplist({G2}/[Triple]>>rdf_assert(Triple, G2), Tree)
    ),
    call(Goal_1, G2),
    (
      hdt_save(G),
      rdf_unload_graph(G2)
    )
  ),
  fail.
hdt_update_tree(_, _).
