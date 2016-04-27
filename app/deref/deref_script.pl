:- module(
  deref_script,
  [
    deref_all/0,
    iri_from_file/1 % -Iri
  ]
).

/** <module> Dereferencing script

@author Wouter Beek
@author Niels Ockeloen
@version 2016/04
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(lists)).
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
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(deref_core).

%:- debug(deref(flag)).

:- dynamic
    deref_thread_iri0/2.





deref_all :-
  flag(deref, _, 1),
  N1 = 75,
  N2 = 75,
  expand_file_name('/ssd/lodlab/wouter/iri_part_*', Sources1),
  length(Sources2, N1),
  append(Sources2, Sources3, Sources1),
  length(Sources4, N2),
  append(Sources4, _, Sources3),
  numlist(1, N2, Ns),
  maplist(start_deref_thread, Ns, Sources4).


start_deref_thread(N, Source) :-
  atomic_list_concat([deref,N], '_', Alias),
  thread_create(run_deref_thread(Source), _, [alias(Alias)]).


run_deref_thread(Source) :-
  file_name_extension(Source, nt, Sink),
  setup_call_cleanup(
    (
      open(Sink, write, Out),
      open(Source, read, In)
    ),
    run_deref_thread_stream(Out, In),
    (
      close(In),
      close(Out)
    )
  ).


run_deref_thread_stream(Out, In) :-
  read_line_to_codes(In, Cs),
  (   Cs == end_of_file
  ->  true
  ;   deref_codes(Out, Cs),
      run_deref_thread_stream(Out, In)
  ).


deref_codes(Out, Cs) :-
  phrase(deref_iri(NumDocs, Iri), Cs),
  update_deref_thread_iri(Iri),
  deref_iri(Out, Iri),
  rdf_store(Out, Iri, deref:number_of_documents, NumDocs^^xsd:nonNegativeInteger).


update_deref_thread_iri(Iri) :-
  thread_self(Alias),
  with_mutex(deref_thread_iri, (
    retractall(deref_thread_iri0(Alias, _)),
    assert(deref_thread_iri0(Alias, Iri))
  )).


deref_iri(NumDocs, Iri) -->
  integer(NumDocs), " ", rest(Cs), {atom_codes(Iri, Cs)}.



iri_from_file(Iri) :-
  iri_file(File),
  gzopen(File, read, In),
  iri_from_stream(In, Iri).


iri_file('/ssd/lodlab/wouter/sorted_iri.gz').


iri_from_stream(In, Iri) :-
  read_line_to_codes(In, Cs),
  (   Cs == end_of_file
  ->  close(In)
  ;   phrase(deref_iri(_, Iri), Cs),
      iri_from_stream(In, Iri)
  ).
