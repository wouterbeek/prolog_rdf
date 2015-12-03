:- module(rdf_graph_test, []).

/** <module> RDF graph test

Unit testing for RDF graph support.

@author Wouter Beek
@version 2013/10, 2014/01, 2015/12
*/

:- use_module(library(plunit)).

:- begin_tests(rdf_graph).

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(rdf/rdf_build)).

test(rdf_graph_instance, []):-
  maplist(rdf_unload_graph_deb, [ex:'test-graph',ex:'test-graph-instance']),
  maplist(rdf_bnode, [X1,X2,X3,X4]),
  grdf_assert(X1, rdf:p, X2, ex:'test-graph'),
  grdf_assert(X3, rdf:p, X4, ex:'test-graph'),
  grdf_assert(rdf:a, rdf:p, rdf:b, ex:'test-graph-instance'),
  grdf_assert(rdf:c, rdf:p, rdf:d, ex:'test-graph-instance'),
  findall(
    Map,
    (
      rdf_graph_instance(ex:'test-graph-instance', ex:'test-graph', Map),
      dcg_with_output_to(user_output, (list(pl_term, Map), nl))
    ),
    _Maps
  ).

:- end_tests(rdf_graph).
