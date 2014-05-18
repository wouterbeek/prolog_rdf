:- module(
  rdf_randomize_iris,
  [
    rdf_randomize_iris/1 % +Graph:atom
  ]
).

/** <module> RDF randomize IRIs

@author Wouter Beek
@version 2014/01
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(meta_ext)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(rdf_term(rdf_term)).



rdf_randomize_iris(G):-
  rdf_graph(G),
  aggregate_all(
    set(IRI1-IRI2),
    (
      rdf_iri(IRI1, G),
      once(randomize_iri(IRI1, IRI2))
    ),
    Dict
  ),
  findall(
    S-P-O,
    rdf(S, P, O, G),
    Triples
  ),
  maplist(randomize_triple(G, Dict), Triples).

random_character -->
  {random_between(97, 122, X)},
  [X].

randomize_iri(IRI1, IRI2):-
  uri_components(IRI1, uri_components(_, _, _, _, _)),
  dcg_with_output_to(atom(Path1), dcg_multi(random_character, 15)),
  atomic_concat('/', Path1, Path2),
  uri_components(IRI2, uri_components(http, 'vu.nl', Path2, _, _)).

randomize_triple(Graph, Dict, S1-P1-O1):-
  rdf_retractall(S1, P1, O1, Graph),
  maplist(iri_lookup(Dict), [S1,P1,O1], [S2,P2,O2]),
  rdf_assert(S2, P2, O2, Graph).

iri_lookup(Dict, X, Y):-
  memberchk(X-Y, Dict), !.
iri_lookup(_, X, X).

