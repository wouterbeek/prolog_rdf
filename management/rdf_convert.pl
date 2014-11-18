:- module(
  rdf_convert,
  [
    rdf_convert_directory/4, % +Directory:atom
                             % ?ToFormat:atom
                             % -ToFiles:list(atom)
                             % +Options:list(nvpair)
    rdf_convert_directory/5, % +FromDirectory:atom
                             % +ToDirectory:atom
                             % ?ToFormat:atom
                             % -ToFiles:list(atom)
                             % +Options:list(nvpair)
    rdf_convert_file/5 % +FromFormat:atom
                       % +FromFile:atom
                       % ?ToFormat:atom
                       % ?ToFile:atom
                       % +Options:list(nvpair)
  ]
).

/** <module> RDF convert

Predicates for converting RDF data between different serialization formats.

@author Wouter Beek
@version 2014/04-2014/05, 2014/07
*/

:- use_module(library(lists), except([delete/3])).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(meta_ext)).
:- use_module(os(file_ext)).

:- use_module(plRdf(rdf_meta)).
:- use_module(plRdf(management/rdf_file)).
:- use_module(plRdf(management/rdf_file_db)).

:- predicate_options(rdf_convert_directory/4, 4, [
     pass_to(rdf_convert_directory/5, 5)
   ]).
:- predicate_options(rdf_convert_directory/5, 5, [
     pass_to(rdf_convert_file/5, 5)
   ]).
:- predicate_options(rdf_convert_file/5, 5, [
     overwrite(+boolean),
     pass_to(rdf_setup_call_cleanup/5, 4),
     pass_to(rdf_setup_call_cleanup/5, 5)
   ]).



%! rdf_convert_directory(
%!   +Directory:atom,
%!   ?ToFormat:atom,
%!   -ToFiles:list(atom),
%!   +Options:list(nvpair)
%! ) is det.
% @see Wrapper around rdf_convert_directory/5,
%      where the results of conversion are stored in the same directory.

rdf_convert_directory(Dir, ToFormat, ToFiles, Options):-
  rdf_convert_directory(Dir, Dir, ToFormat, ToFiles, Options).

%! rdf_convert_directory(
%!   +FromDirectory:atom,
%!   +ToDirectory:atom,
%!   ?ToFormat:atom,
%!   -ToFiles:list(atom),
%!   +Options:list(nvpair)
%! ) is det.
% Converts the RDF files in one directory into new RDF files
% in another directory.
%
% The following options are supported:
%   * =|overwrite(+boolean)|=
%     Whether RDF files that have been converted to a new file
%     should be overwritten.
%     Default: `false`.

rdf_convert_directory(FromDir, ToDir, ToFormat, ToFiles, Options):-
  rdf_directory_files(FromDir, FromFiles),

  default(ntriples, ToFormat),
  rdf_file_extension_format(ToExtension, ToFormat),

  findall(
    ToFile,
    (
      member(FromFile, FromFiles),
      file_alternative(FromFile, ToDir, _, ToExtension, ToFile),
      rdf_convert_file(_, FromFile, ToFormat, ToFile, Options)
    ),
    ToFiles
  ).


%! rdf_convert_file(
%!   ?FromFormat:atom,
%!   +FromFile:atom,
%!   +ToFormat:atom,
%!   ?ToFile:atom,
%!   +Options:list(nvpair)
%! ) is det.
% Converts the given RDF file to the RDF serialization format
% with the given MIME content type.
%
% The following options are supported:
%   * =|overwrite(+boolean)|=
%     Whether RDF files that have been converted to a new file
%     should be overwritten.
%     Default: `false`.

rdf_convert_file(FromFormat, FromFile, ToFormat, ToFile, Options):-
  % If the RDF serialization format of the current RDF file is given,
  % then this can be used in loading the file.
  (
    var(FromFormat)
  ->
    LoadOptions = []
  ;
    LoadOptions = [format(FromFormat)]
  ),
  SaveOptions = [format(ToFormat)],
  
  % We use the RDF meta predicate module in order to load and save the data.
  % The goal argument rdf_graph/1 succeeds trivially.
  rdf_setup_call_cleanup(
    FromFile,
    rdf_graph,
    ToFile,
    LoadOptions,
    SaveOptions
  ),
  
  % Process the `replace` option.
  (
    FromFile == ToFile
  ->
    true
  ;
    option(overwrite(true), Options)
  ->
    delete_file(FromFile)
  ;
    true
  ).

