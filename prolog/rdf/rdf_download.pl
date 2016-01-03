:- module(
  rdf_download,
  [
    rdf_download/2, % +Iri, ?File
    rdf_download/3 % +Iri:iri
                   % ?File:atom
                   % +Options:list(compound)
  ]
).

/** <module> RDF download

Download RDF data to file.

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(uri/uri_file_name)).

:- predicate_options(rdf_download/3, 3, [
     pass_to(rdf_read_from_stream/3, 3)
   ]).





%! rdf_download(+Iri, ?File:atom) is det.
% Wrapper around rdf_download/3 with default options.

rdf_download(Iri, File) :-
  rdf_download(Iri, File, []).


%! rdf_download(+Iri, ?File:atom, +Options:list(compound)) is det.

rdf_download(Iri, File, Opts) :-
  option(metadata(M), Opts, _),
  (var(File) -> nested_uri_file_name(Iri, File) ; true),
  thread_file(File, TmpFile),
  rdf_read_from_stream(Iri, write_stream_to_file0(TmpFile, M), Opts),
  rename_file(TmpFile, File).

write_stream_to_file0(File, M, M, Read) :-
  write_stream_to_file(Read, File).
