:- module(
  rdf_convert,
  [
    rdf_convert_directory/4, % +FromDirectory:atom
                             % +ToDirectory:atom
                             % ?ToMIME:atom
                             % -ToFiles:list(atom)
    rdf_convert_file/4 % +FromMIME:atom
                       % +FromFile:atom
                       % ?ToMIME:atom
                       % ?ToFile:atom
  ]
).

/** <module> RDF convert

Predicates for converting RDF data between different serialization formats.

@author Wouter Beek
@version 
*/

:- use_module(generics(meta_ext)).
:- use_module(library(lists)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf_file(rdf_file)).



%! rdf_convert_directory(
%!   +FromDirectory:atom,
%!   +ToDirectory:atom,
%!   ?ToMIME:atom,
%!   -ToFiles:list(atom)
%! ) is det.

rdf_convert_directory(FromDir, ToDir, ToMime, ToFiles):-
  rdf_directory_files(FromDir, FromFiles),

  default('application/x-turtle', ToMime),
  once((
    rdf_serialization(ToExt, _, _, Mimes, _),
    memberchk(ToMime, Mimes)
  )),

  findall(
    ToFile,
    (
      member(FromFile, FromFiles),
      file_alternative(FromFile, ToDir, _, ToExt, ToFile),
      rdf_convert_file(_, FromFile, ToMime, ToFile)
    ),
    ToFiles
  ).


%! rdf_convert_file(
%!   ?FromMIME:atom,
%!   +FromFile:atom,
%!   +SaveOptions:atom,
%!   ?ToFile:atom
%! ) is det.

rdf_convert_file(FromMIME, FromFile, ToMIME, ToFile):-
  (
    var(FromMIME)
  ->
    LoadOptions = []
  ;
    LoadOptions = [mime(FromMIME)]
  ),
  rdf_setup_call_cleanup(
    LoadOptions,
    FromFile,
    rdf_graph,
    [mime(ToMIME)],
    ToFile
  ).

