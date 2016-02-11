:- module(
  rdf_store,
  [
    rdf_store/3, % +S, +P, +O
    rdf_store/4  % +S, +P, +O, ?G
  ]
).

/** <module> RDF store

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(apply)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(simple/write_SimpleRDF)).

:- rdf_meta
   rdf_store(r,r,o),
   rdf_store(r,r,o,r).





rdf_store(S1, P1, O1) :-
  maplist(rdf_alias, [S1,P1,O1], [S2,P2,O2]),
  write_simple_triple(S2, P2, O2).

rdf_store(S1, P1, O1, G1) :-
  maplist(rdf_alias, [S1,P1,O1,G1], [S2,P2,O2,G2]),
  write_simple_triple(S2, P2, O2, G2).

rdf_alias(Alias-Local, Iri) :- !,
  rdf_global_id(Alias:Local, Iri).
rdf_alias(Iri, Iri).
