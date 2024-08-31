:- encoding(utf8).
:- module(
  rdf_download,
  [
    rdf_download/1, % +Uri
    rdf_download/2, % +Uri, ?File
    rdf_download/3  % +Uri, ?File, +Options
  ]
).

/** <module> RDF download

*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(http_client2)).
:- use_module(library(semweb/rdf_clean)).
:- use_module(library(semweb/rdf_deref)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(uri_ext)).



%! rdf_download(+Uri:uri) is det.
%! rdf_download(+Uri:uri, ?File:atom) is det.
%! rdf_download(+Uri:uri, ?File:atom, +Options:options) is det.
%
% @param Options are passed to rdf_deref_uri/3.

rdf_download(Uri) :-
  rdf_download(Uri, _).


rdf_download(Uri, File) :-
  rdf_download(Uri, File, options{}).


rdf_download(Uri, File, Options) :-
  ensure_file_(Uri, File),
  (   uri_file_is_fresh(Uri, File)
  ->  true
  ;   write_to_file(
        File,
        {Uri,Options}/[Out0]>>rdf_deref_uri(Uri, callback_(Out0), Options)
      )
  ).

callback_(Out, Site, Tuples1, _) :-
  convlist(rdf_clean_tuple(Site), Tuples1, Tuples2),
  maplist(rdf_write_tuple(Out), Tuples2).

ensure_file_(_, File) :-
  ground(File), !.
ensure_file_(Uri, File) :-
  uri_data_file(Uri, data, File).
