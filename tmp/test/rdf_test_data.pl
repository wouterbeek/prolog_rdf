:- module(
  rdf_test_data,
  [
    qb_test/3 % +M, ?Name, ?G
  ]
).

/** <module> RDF test data

@author Wouter Beek
@version 2015/12-2016/01, 2016/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(rdf/rdf_build)).

:- rdf_create_alias(ch, 'http://www.wouterbeek.com/ch.owl#' ).
:- rdf_create_alias(dbr, 'http://dbpedia.org/resource/').
:- rdf_create_alias(ex, 'http://example.org/').
:- rdf_create_alias(nl, 'http://www.wouterbeek.com/nl.owl#' ).





qb_test(M, database(identity), G) :-
  rdf_equal(ex:identity, G),
  qb_label(M, ex:a, "a", G),
  qb_label(M, ex:b, "b", G),
  qb(M, ex:a, owl:sameAs, ex:b, G),
  qb(M, ex:a, owl:sameAs, ex:c, G),
  qb(M, ex:d, owl:sameAs, ex:c, G).



qb_test(M, modeling(visum), G) :-
  % Use a default graph name if none is given.
  rdf_equal(ex:visum, G),

  % Chinese namespace
  qb_subclass(M, ch:capital, ch:cityWithAirport, G),
  qb_instance(M, ch:'Amsterdam', ch:capital, G),
  qb_subclass(M, ch:europeanCity, ch:visumNeeded, G),
  qb_instance(M, ch:'Amsterdam', ch:europeanCity, G),

  % Dutch namespace
  qb_subclass(M, nl:visumFree, nl:europeanCity, G),
  qb_instance(M, nl:'Amsterdam', nl:europeanCity, G),
  qb_instance(M, nl:'Amsterdam', nl:capital, G),

  % Interrelations
  qb_equivalent_class(ch:capital, nl:capital, G),
  qb(M, dbr:'Amsterdam', owl:sameAs, ch:'Amsterdam', G),
  qb(M, dbr:'Amsterdam', owl:sameAs, nl:'Amsterdam', G).


qb_test(M, semantics(graph_instance), _) :-
  maplist(q_unload, [ex:'test-graph',ex:'test-graph-instance']),
  maplist(qb_bnode, [X1,X2,X3,X4]),
  qb(M, X1, rdf:p, X2, ex:'test-graph'),
  qb(M, X3, rdf:p, X4, ex:'test-graph'),
  qb(M, rdf:a, rdf:p, rdf:b, ex:'test-graph-instance'),
  qb(M, rdf:c, rdf:p, rdf:d, ex:'test-graph-instance').
  %forall(
  %  rdf_graph_instance(ex:'test-graph-instance', ex:'test-graph', Map),
  %  dcg_with_output_to(user_output, (list(term, Map), nl))
  %).
