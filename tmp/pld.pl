:- module(pld, [run_pld/0]).

:- use_module(library(os/io)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(service/ll_api)).
:- use_module(library(uri)).

:- rdf_create_alias(llo, 'http://lodlaundromat.org/ontology/').

run_pld :-
  call_to_stream('pld.csv', write_pld).

write_pld(Out) :-
  Query = '\c
PREFIX llo: <http://lodlaundromat.org/ontology/>\n\c
SELECT ?uri ?doc\n\c
WHERE {\n\c
  ?doc llo:url ?url\n\c
}\n',
  ll_sparql_select(Query, Rows),
  member([Uri,Doc], Rows),
  uri_components(Uri, uri_components(_,Auth,_,_,_)),
  format(Out, "~a,~a~n", [Auth,Doc]),
  flush_output(Out),
  fail.
writel_pld(_).
