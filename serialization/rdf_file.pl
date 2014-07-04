:- module(
  rdf_file,
  [
    is_rdf_file/1, % +File:atom
    rdf_file_correct_extension/2, % +FromFile:atom
                                  % -ToFile:atom
    rdf_directory_files/2, % +Directory:atom
                           % -Files:list(atom)
    rdf_merge_directory/4 % +FromDirectory:atom
                          % +ToFile:atom
                          % +LoadOptions:list(nvpair)
                          % +SaveOptions:list(nvpair)
  ]
).

/** <module> RDF file

Support for RDF files and file types.

@author Wouter Beek
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09, 2013/11, 2014/01-2014/05
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(file_mime)).

:- use_module(plRdf(rdf_meta)).
:- use_module(plRdf_ser(rdf_file_db)).

:- predicate_options(rdf_merge_directory/4, 3, [
     pass_to(rdf_setup_call_cleanup/5, 4)
   ]).
:- predicate_options(rdf_merge_directory/4, 4, [
     pass_to(rdf_setup_call_cleanup/5, 5)
   ]).



%! is_rdf_file(+File:atom) is semidet.
% Succeeds if the given file contains an RDF serialization.

is_rdf_file(File):-
  file_mime(File, Mime),
  rdf_mime(Mime), !.
is_rdf_file(File):-
  file_name_extension(_, Extension, File),
  rdf_extension(Extension).


%! rdf_directory_files(+Directory:atom, -RdfFiles:list(atom)) is det.
%! rdf_directory_files(
%!   +Options:list(nvpair),
%!   +Directory:atom,
%!   -RdfFiles:list(atom)
%! ) is det.
% Returns RDF files from the given directory.
% This is based on parsing (the top of) the contents of these files.
%
% @arg Options Passed to directory_files/3.
% @arg Directory The atomic name of a directory.
% @arg RdfFiles A list of atomic file names of RDF files.

rdf_directory_files(Dir, RdfFiles):-
  rdf_directory_files(
    [include_directories(false),include_self(false),recursive(true)],
    Dir,
    RdfFiles
  ).

rdf_directory_files(O1, Dir, RdfFiles):-
  % Retrieve all files.
  directory_files(O1, Dir, Files),
  include(is_rdf_file, Files, RdfFiles).


%! rdf_file_correct_extension(+FromFile:atom, -ToFile:atom) is det.

rdf_file_correct_extension(File1, File2):-
  file_mime(File1, Mime),
  rdf_serialization(Extension, _, _, Mimes, _),
  memberchk(Mime, Mimes),
  file_alternative(File1, _, _, Extension, File2),
  File1 \== File2, !,
  rename_file(File1, File2).
rdf_file_correct_extension(File, File).


%! rdf_merge_directory(
%!   +FromDirectory:atom,
%!   +ToFile:atom,
%!   +LoadOptions:list(nvpair),
%!   +SaveOptions:list(nvpair)
%! ) is det.

rdf_merge_directory(FromDir, ToFile, LoadOptions, SaveOptions):-
  rdf_directory_files(FromDir, FromFiles),
  FromFiles \== [],
  rdf_setup_call_cleanup(
    FromFiles,
    rdf_graph,
    ToFile,
    LoadOptions,
    SaveOptions
  ).

