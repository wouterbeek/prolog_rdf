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





deref_all :-
  flag(deref, _, 1),
  expand_file_name('/ssd/lodlab/wouter/iri_part_*', Files),
  setup_call_cleanup(
    gzopen('/ssd/lodlab/wouter/deref.nt', write, Out, [alias(deref)]),
    concurrent_maplist(deref_file(deref), Files),
    close(Out)
  ).


deref_file(Out, File) :-
  setup_call_cleanup(
    open(File, read, In),
    deref_stream(In, Out),
    close(In)
  ).


deref_stream(In, Out) :-
  read_line_to_codes(In, Cs),
  (   Cs == end_of_file
  ->  true
  ;   deref_codes(Out, Cs),
      deref_stream(In, Out)
  ).


deref_codes(Out, Cs) :-
  phrase(deref_iri(NumDocs, Iri), Cs),
  flag(deref, X, X + 1), debug(deref(flag), "~D  ~t  ~a", [X,Iri]),
  deref_iri(Out, Iri),
  rdf_store(Out, Iri, deref:number_of_documents, NumDocs^^xsd:nonNegativeInteger).


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
