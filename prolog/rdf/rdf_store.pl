:- module(
  rdf_store,
  [
    rdf_store/2,      % +Sink, +Triple
    rdf_store/4,      % +Sink, +S, +P, +O
    rdf_store_list/3, % +Sink, +L, -RdfL
    rdf_store_now/3   % +Sink, +S, +P
  ]
).

/** <module> RDF store

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(gen/gen_ntuples)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_store(+, t),
   rdf_store(+, r, r, o),
   rdf_store_now(+, r, r).





%! rdf_store(+Sink, +Triple) is det.
%! rdf_store(+Sink, +S, +P, +O) is det.

rdf_store(Out, rdf(S,P,O)) :-
  rdf_store(Out, S, P, O).


rdf_store(Out, S, P, O) :-
  is_stream(Out), !,
  with_output_to(Out, gen_ntriple(S, P, O)).
rdf_store(G, S, P, O) :-
  rdf_assert(S, P, O, G).



%! rdf_store_list(+Out, +L, -RdfL) is det.

rdf_store_list(Out, L, B) :-
  rdf_create_bnode(B),
  rdf_store_list0(Out, B, L).


rdf_store_list0(_, _, []) :- !.
rdf_store_list0(Out, B1, [H|T]) :-
  rdf_store(Out, B1, rdf:first, H),
  (T == [] -> rdf_equal(rdf:nil, B2) ; rdf_create_bnode(B2)),
  rdf_store(Out, B1, rdf:rest, B2),
  rdf_store_list0(Out, B2, T).



%! rdf_store_now(+Out, +S, +P) is det.

rdf_store_now(Out, S, P) :-
  get_time(Now),
  rdf_store(Out, S, P, Now^^xsd:dateTime).
