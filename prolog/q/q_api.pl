:- module(
  q_api,
  [
    t/6 % ?M, ?S, ?P, ?O, ?G, ?D
  ]
).

:- use_module(library(semweb/rdf11)).

:- rdf_meta
   t(?, r, r, o, r, r).

t(hdt, S, P, O, G) :-
  hdt(data, S, P, O, G).

t(M, S, P, O, G, D) :-
  (nonvar(G) ; nonvar(D)), !,
  ignore(rdf_dataset_graph(D, G)),
  t(M, S, P, O, G).
t(M, S, P, O, G, D) :-
  t(M, S, P, O, G),
  ignore(rdf_dataset_graph(D, G)).
