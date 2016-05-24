:- module(
  odc,
  [
    odc_entry/2 % -Res, -Triples
  ]
).

/** <module> Open Data Communities (ODC)

@author Wouter Beek
@version 2014/05, 2016/05
*/

:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_io)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).





%! odc_entry(-Res, -Triples) is nondet.

odc_entry(Res, Triples) :-
  rdf_call_on_graph('http://opendatacommunities.org/data.ttl',
    {Res,Triples}/[G,M,M]>>odc_entry0(Res, Triples, G)
  ).

odc_entry0(Res, Triples, G) :-
  distinct(Res, rdf_subject(G, Res)),
  rdf_triples(Res, _, _, Triples).
