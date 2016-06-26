:- module(
  odc,
  [
    odc_entry/2 % -Res, -Triples
  ]
).

/** <module> Open Data Communities (ODC)

@author Wouter Beek
@version 2014/05, 2016/05-2016/06
*/

:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).
:- use_module(library(z/z_stmt)).
:- use_module(library(z/z_term)).





%! odc_entry(-Res, -Triples) is nondet.

odc_entry(Res, Triples) :-
  rdf_call_on_graph('http://opendatacommunities.org/data.ttl',
    {Res,Triples}/[G,M,M]>>odc_entry0(Res, Triples, G)
  ).


odc_entry0(Res, Triples, G) :-
  distinct(Res, z_subject(G, Res)),
  z_triples(Res, _, _, Triples).
