:- module(
  deref,
  [
    deref_all/0,
    deref_all/1, % +N
    deref_iri/1  % +Iri
  ]
).

/** <module> Dereference application

@author Wouter Beek
@author Niels Ockeloen
@version 2016/04
*/

:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_error)).
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





%! deref_all is det.
%! deref_all(+N) is det.

deref_all :-
  default_number_of_threads(N),
  deref_all(N).


deref_all(N) :-
  flag(deref, _, 0),
  setup_call_cleanup(
    gzopen('/scratch/lodlab/crawls/iri.gz', read, In),
    deref_all(In, N),
    close(In)
  ).


deref_all(In, N) :-
  % Remove the first line which contains RocksDB rubbish.
  read_line_to_codes(In, _),
  deref_all_nonfirst(In, N).


deref_all_nonfirst(In, N) :-
  read_lines(In, N, Lines),
  (   Lines == []
  ->  true
  ;   concurrent_maplist(deref_line, Lines),
      deref_all_nonfirst(In, N)
  ).


deref_graph(Iri, G) :-
  rdf_print_graph(G), nl,
  rdf_aggregate_all(count, rdf(Iri, _, _, G), NS),
  format(user_output, "Appears in subject position: ~D~n", [NS]),
  rdf_aggregate_all(count, rdf(_, Iri, _, G), NP),
  format(user_output, "Appears in predicate position: ~D~n", [NP]),
  rdf_aggregate_all(count, rdf(_, _, Iri, G), NO),
  format(user_output, "Appears in object position: ~D~n", [NO]).


deref_iri(Iri) :-
  flag(deref, X, X + 1),
  format(user_output, "~D~n", [X]),
  (X = -1 -> gtrace ; true),
  Opts = [metadata(M),parse_headers(true),triples(NumTriples),quads(NumQuads)],
  (   catch(rdf_call_on_graph(Iri, deref_graph(Iri), Opts), E, writeln(E))
  ->  (   var(E)
      ->  print_dict(M),
          msg_success("Number of triples: ~D~n", [NumTriples]),
          msg_success("Number of quads: ~D~n", [NumQuads])
      ;   store_exception(Iri, E)
      ->  true
      ;   print_message(E, error)
      )
  ;   msg_warning("HOPEFULLY THE ARCHIVE_CLOSE/1 HACK...~n")
  ).


deref_iri(Iri, NumDocs) :-
  deref_iri(Iri),
  format(user_output, "Number of documents: ~D~n", [NumDocs]).


deref_iri(NumDocs, Iri) -->
  integer(NumDocs), " ", rest(Cs), {atom_codes(Iri, Cs)}.



deref_line(Cs) :-
  phrase(deref_iri(NumDocs, Iri), Cs),
  deref_iri(Iri, NumDocs).





% DEBUG %

iri('http://dbpedia.org/resource/Tim_Berners-Lee').





% HELPERS %

read_lines(_, 0, []) :- !.
read_lines(In, N1, L) :-
  read_line_to_codes(In, H),
  (   H == end_of_file
  ->  L = []
  ;   N2 is N1 - 1,
      L = [H|T],
      read_lines(In, N2, T)
  ).



store_exception(Iri, E) :-
  setup_call_cleanup(
    open('warn.nt', write, Out),
    rdf_store_warning(Out, Iri, E),
    close(Out)
  ).
