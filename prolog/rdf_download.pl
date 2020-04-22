:- module(
  rdf_download,
  [
    rdf_download/1, % +Uri
    rdf_download/2  % +Uri, ?File
  ]
).

/** <module> RDF download

*/

:- use_module(library(apply)).

:- use_module(library(file_ext)).
:- use_module(library(rdf_clean)).
:- use_module(library(rdf_deref)).
:- use_module(library(rdf_export)).
:- use_module(library(uri_ext)).





%! rdf_download(+Uri:atom) is det.
%! rdf_download(+Uri:atom, ?File:atom) is det.

rdf_download(Uri) :-
  rdf_download(Uri, _).


rdf_download(Uri, File) :-
  (var(File) -> uri_data_file(Uri, File) ; true),
  (   exists_file(File),
      file_size(File, Size),
      Size =\= 0
  ->  true
  ;   write_to_file(File, rdf_download_(Uri))
  ).

rdf_download_(Uri, Out) :-
  rdf_deref_uri(Uri, callback_(Out)).

callback_(Out, Site, Tuples1, _) :-
  convlist(rdf_clean_tuple(Site), Tuples1, Tuples2),
  maplist(rdf_write_tuple(Out), Tuples2).
