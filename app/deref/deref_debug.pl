:- module(
  deref_debug,
  [
    deref_iri/1,         % +Iri
    deref_iri_to_file/1, % +Iri
    deref_iri_to_file/2, % +Iri, +File
    iri/1                % -Iri
  ]
).

/** <module> Debug dereferencing

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).
:- use_module(library(yall)).

:- use_module(deref_core).
:- use_module(deref_script).





%! deref_iri(+Iri) is det.

deref_iri(Iri) :-
  rdf_snap((
    deref_iri(Iri, Iri),
    rdf_print_graph(Iri),
    rdf_unload_graph(Iri)
  )).



%! deref_iri_to_file(+Iri) is det.
%! deref_iri_to_file(+Iri, +File) is det.

deref_iri_to_file(Iri) :-
  uri_components(Iri, uri_components(_,Auth,Path,_,_)),
  atomic_list_concat(Subpaths, /, Path),
  atomic_list_concat([Auth|Subpaths], '_', Base),
  file_name_extension(Base, nt, File),
  deref_iri_to_file(Iri, File).


deref_iri_to_file(Iri, File) :-
  call_to_stream(File, {Iri}/[Out,M,M]>>deref_iri(Out, Iri)).



iri('http://0-0-7.livejournal.com/data/rss').
iri('http://%20ossiane.blog@studio-amarante.com/').
iri('http://dbpedia.org/resource/Tim_Berners-Lee').
iri('http://%5Cdementialcore.blogspot.com').
iri(Iri) :-
  iri_from_file(Iri).
