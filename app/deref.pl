:- module(deref, [deref1/0,deref1/1,deref2/0,deref2/1]).

:- use_module(library(dcg/dcg_ext), []).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).

:- dcg_ext:set_setting(tab_size, 2).

deref1 :- rdf_test(Iri), deref1(Iri).
deref2 :- rdf_test(Iri), deref2(Iri).

deref1(Iri) :- rdf_read_from_stream(Iri, deref1, [parse_headers(true)]).

deref1(M, Source) :-
  print_dict(M, [indent(2)]),
  peek_string(Source, 100, String),
  format(user_output, "~s~n", [String]),
  rdf_load:rdf_call_on_tuples_stream(deref:rdf_print_quad0, [], M, Source).
rdf_print_quad0(_, S, P, O, G) :- rdf_print_quad(S, P, O, G), nl.

deref2(Iri) :-
  rdf_call_on_graph(Iri, rdf_print_graph, [metadata(M),parse_headers(true)]),
  nl,
  print_dict(M).

rdf_test('http://dbpedia.org/resource/Tim_Berners-Lee').
