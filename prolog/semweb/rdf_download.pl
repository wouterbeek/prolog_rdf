:- module(
  rdf_download,
  [
    rdf_download/1, % +Uri
    rdf_download/2  % +Uri, +File
  ]
).

/** <module> RDF download

@author Wouter Beek
@version 2019
*/

:- use_module(library(apply)).

:- use_module(library(file_ext)).
:- use_module(library(semweb/rdf_clean)).
:- use_module(library(semweb/rdf_deref)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(uri_ext)).





%! rdf_download(+Uri:atom) is det.
%! rdf_download(+Uri:atom, +File:atom) is det.

rdf_download(Uri) :-
  uri_local_name(Uri, Local),
  file_name(Local, Name),
  file_name_extensions(File, Name, [nq,gz]),
  rdf_download(Uri, File).


rdf_download(Uri, File) :-
  write_to_file(File, rdf_download_(Uri)).

rdf_download_(Uri, Out) :-
  rdf_deref_uri(Uri, callback_(Out)).

callback_(Out, Site, Tuples1, _) :-
  convlist(rdf_clean_tuple(Site), Tuples1, Tuples2),
  maplist(rdf_write_tuple(Out), Tuples2).
