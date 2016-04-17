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
:- use_module(library(debug)).
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

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- dcg_ext:set_setting(tab_size, 2).

:- debug(deref).





%! deref_all is det.
%! deref_all(+N) is det.

deref_all :-
  default_number_of_threads(N),
  deref_all(N).


deref_all(N) :-
  flag(deref, _, 1),
  setup_call_cleanup(
    (
      gzopen('/scratch/lodlab/crawls/iri.gz', read, In),
      gzopen('/scratch/lodlab/crawls/deref.nt.gz', write, Out)
    ),
    deref_all(In, Out, N),
    (
      close(Out),
      close(In)
    )
  ).


deref_all(In, Out, N) :-
  % Remove the first line which contains RocksDB rubbish.
  read_line_to_codes(In, _),
  deref_all_nonfirst(In, Out, N).


deref_all_nonfirst(In, Out, N) :-
  read_lines(In, N, Lines),
  (   Lines == []
  ->  true
  ;   concurrent_maplist(deref_line(Out), Lines),
      deref_all_nonfirst(In, Out, N)
  ).



deref_graph(Out, Iri, M, G) :-
  rdf_print_graph(G), nl,
  
  % Number of occurrences in the subject position
  rdf_aggregate_all(count, rdf(Iri, _, _, G), NumSubjects),
  rdf_store(Out, Iri, deref:number_of_subjects, NumSubjects^^xsd:nonNegativeInteger),
  debug(deref, "Appears in subject position: ~D", [NumSubjects]),

  % Number of occurrences in the predicate position
  rdf_aggregate_all(count, rdf(_, Iri, _, G), NumPredicates),
  rdf_store(Out, Iri, deref:number_of_subjects, NumPredicates^^xsd:nonNegativeInteger),
  debug(deref, "Appears in predicate position: ~D", [NumPredicates]),

  % Number of occurrences in the object position
  rdf_aggregate_all(count, rdf(_, _, Iri, G), NumObjects),
  rdf_store(Out, Iri, deref:number_of_subjects, NumObjects^^xsd:nonNegativeInteger),
  debug(deref, "Appears in object position: ~D", [NumObjects]),

  % Metadata
  print_dict(M). %TBD



deref_iri(Iri) :-
  deref_iri(user_output, Iri).


deref_iri(Out, Iri) :-
  % Debug index
  flag(deref, X, X + 1),
  debug(deref, "~D", [X]),
  (X = 428607 -> gtrace ; true),
  Opts = [
    base_iri(Iri),
    triples(NumTriples),
    quads(NumQuads)
  ],
  (   catch(rdf_call_on_graph(Iri, deref_graph(Out, Iri), Opts), E, true)
  ->  (   var(E)
      ->  % Number of triples
	  rdf_store(Out, Iri, deref:number_of_triples, NumTriples^^xsd:nonNegativeInteger),
          debug(deref, "Number of triples: ~D", [NumTriples]),
	  % Number of quadruples
	  rdf_store(Out, Iri, deref:number_of_quads, NumQuads^^xsd:nonNegativeInteger),
          debug(deref, "Number of quads: ~D", [NumQuads])
      ;   % Exception
          rdf_store_warning(Out, Iri, E)
      )
  ;   msg_warning("HOPEFULLY THE ARCHIVE_CLOSE/1 HACK...~n") %TBD
  ).


deref_iri(Out, Iri, NumDocs) :-
  deref_iri(Out, Iri),
  rdf_store(Out, Iri, deref:number_of_documents, NumDocs^^xsd:nonNegativeInteger),
  debug(deref, "Number of documents: ~D", [NumDocs]).


deref_iri(NumDocs, Iri) -->
  integer(NumDocs), " ", rest(Cs), {atom_codes(Iri, Cs)}.



deref_line(Out, Cs) :-
  phrase(deref_iri(NumDocs, Iri), Cs),
  deref_iri(Out, Iri, NumDocs).





% DEBUG %

iri('http://dbpedia.org/resource/Tim_Berners-Lee').
iri('http://%20ossiane.blog@studio-amarante.com/').





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
