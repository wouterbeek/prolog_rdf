:- module(
  rdfs_proof_gif,
  [
    rdfs_proof_gif/5 % ?Subject:or([bnode,iri])
                     % ?Predicate:iri
                     % ?Object:or([bnode,iri,literal])
                     % +Graph:atom
                     % -Gif:compound
  ]
).

/** <module> RDFS proof GIF

Generates proof trees for RDFS entailment using the Graph Interchange Format.

@author Wouter Beek
@version 2014/03, 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plDcg(dcg_generics)).

:- use_module(plRdf(rdf_name)).
:- use_module(plRdf_ent(rdfs_proof)).

:- rdf_meta(rdfs_proof(r,r,o,+,-)).



rdf_proof_gif(S, P, O, G, Gif):-
  rdfs_proof(S, P, O, G, Tree),
  flag(vertex, _, 0),
  tree_to_ves(Tree, Vs, Es, _),
  length(Es, L),
  L > 4,
  ves_to_ugraph(Vs, Es, Gif).


tree_to_ves(d(Rule,V1,Ants), Vs, Es, V1):- !,
  maplist(tree_to_ves, Ants, Vss, Ess, VTops),
  findall(
    V1-Rule-V2,
    member(V2, VTops),
    ETops
  ),
  ord_union([[V1]|Vss], Vs),
  ord_union([ETops|Ess], Es).
tree_to_ves(V, [V], [], V).


ves_to_ugraph(Vs, Es, graph(VTerms, ETerms, [dir(forward)])):-
  maplist(v_to_vterm(Vs), Vs, VTerms),
  maplist(e_to_eterm(Vs), Es, ETerms).


e_to_eterm(Vs, V1-Rule-V2, edge(Id1,Id2,[label(Rule)])):-
  nth0(Id1, Vs, V1),
  nth0(Id2, Vs, V2).


v_to_vterm(Vs, rdf(S,P,O), vertex(Id,rdf(S,P,O),[label(Label)])):-
  nth0(Id, Vs, rdf(S,P,O)),
  dcg_with_output_to(atom(Label), rdf_triple_name(rdf(S,P,O))).

