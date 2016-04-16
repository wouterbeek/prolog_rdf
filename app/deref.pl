:- module(deref, [deref_all/1,deref/1]).

:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(thread)).
:- use_module(library(zlib)).

:- dcg_ext:set_setting(tab_size, 2).



deref_all(N) :-
  setup_call_cleanup(
    gzopen('/scratch/lodlab/crawls/13/iri.gz', read, Source),
    deref_all(Source, N),
    close(Source)
  ).


deref_all(Source, N) :-
  % Remove the first line which contains RocksDB rubbish.
  read_line_to_codes(Source, _),
  deref_all_nonfirst(Source, N).


deref_all_nonfirst(Source, N) :-
  read_lines(Source, N, Lines),
  (   Lines == []
  ->  true
  ;   concurrent_maplist(deref_line, Lines),
      sleep(10),
      deref_all_nonfirst(Source, N)
  ).


deref_line(Cs) :-
  phrase(deref_iri(NumDocs, Iri), Cs),
  deref(Iri, NumDocs).


deref_iri(NumDocs, Iri) -->
  integer(NumDocs), " ", rest(Cs), {atom_codes(Iri, Cs)}.


deref(Iri) :-
  Opts = [metadata(M),parse_headers(true),triples(NumTriples),quads(NumQuads)],
  rdf_call_on_graph(Iri, deref_graph(Iri), Opts),
  %print_dict(M),
  format(user_output, "Number of triples: ~D~n", [NumTriples]),
  format(user_output, "Number of quads: ~D~n", [NumQuads]).


deref(Iri, NumDocs) :-
  deref(Iri),
  format(user_output, "Number of documents: ~D~n", [NumDocs]).


deref_graph(Iri, G) :-
  rdf_print_graph(G), nl,
  rdf_aggregate_all(count, rdf(Iri, _, _, G), NS),
  format(user_output, "Appears in subject position: ~D~n", [NS]),
  rdf_aggregate_all(count, rdf(_, Iri, _, G), NP),
  format(user_output, "Appears in predicate position: ~D~n", [NP]),
  rdf_aggregate_all(count, rdf(_, _, Iri, G), NO),
  format(user_output, "Appears in object position: ~D~n", [NO]).



% DEBUG %

iri('http://dbpedia.org/resource/Tim_Berners-Lee').



% HELPERS %

read_lines(_, 0, []) :- !.
read_lines(Source, N1, L) :-
  read_line_to_codes(Source, H),
  (   H == end_of_file
  ->  L = []
  ;   N2 is N1 - 1,
      L = [H|T],
      read_lines(Source, N2, T)
  ).
