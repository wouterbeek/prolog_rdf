:- module(
  rdf_download,
  [
    rdf_download/2, % +Iri, ?File
    rdf_download/3  % +Iri, ?File, +Opts
  ]
).

/** <module> RDF download

Download RDF data to file.

@author Wouter Beek
@version 2015/11, 2016/01
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
%
% @throws existence_error if an HTTP request returns an error code.

rdf_download(Iri, File) :-
  rdf_download(Iri, File, []).


%! rdf_download(+Iri, ?File:atom, +Options:list(compound)) is det.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_download(Iri, File, Opts) :-
  option(metadata(_), Opts, _),
  (var(File) -> uri_file_name_nested(Iri, File) ; true),
  thread_file(File, TmpFile),
  rdf_read_from_stream(Iri, write_stream_to_file0(TmpFile), Opts),
  rename_file(TmpFile, File).

write_stream_to_file0(TmpFile, _, Read) :-
  write_stream_to_file(Read, TmpFile).
