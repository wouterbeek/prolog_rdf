:- module(
  rdf_save_any,
  [
    rdf_save_any/2 % ?File:atom
                   % +Options:list(nvpair)
  ]
).

/** <module> RDF: save any

@author Wouter Beek
@version 2014/10
*/

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(uri)).

:- use_module(os(file_ext)).



%! rdf_save_any(?File:atom, +Options:list(nvpair)) is det.
% If the file name is not given, then a file name is construed.
% There are two variants here:
%   1. The graph was loaded from a file. Use the same file
%      (if we have write access) to this file.
%   2. Make up the file name based on the given graph name.
%      If the format is specified as well, then this is used to determine
%      the file extension.
%
% @throws instantiation_error If (1) File is uninstantiated and
%         (2) there is no `graph` option that has a file associated to it.

% 1.
% File name derived from graph.
% This only works if a graph option is given
% and the denoted graph was loaded form file.
rdf_save_any(File2, Options):-
  var(File2), !,
  (   option(graph(Graph), Options),
      rdf_graph_property(Graph, source(File1))
  ->  uri_file_name(File1, File2),
      create_file(File2),
      rdf_save_any(File2, Options)
  ;   instantiation_error(File2)
  ).

% 2.
% No modifications.
rdf_save_any(File, Options):-
  % We do not need to save the graph if
  % (1) the contents of the graph did not change, and
  % (2) the serialization format of the graph did not change.
  %
  % Make sure the contents of the graph were not changed.
  option(graph(Graph), Options),
  rdf_graph_property(Graph, modified(false)),
  
  % Make sure the file is the same.
  rdf_graph_property(Graph, source(FromFile1)),
  uri_file_name(FromFile1, FromFile2),
  FromFile2 == File,
  
  % The file was not modified after the graph was loaded.
  rdf_graph_property(Graph, source_last_modified(LastModified)),
  exists_file(File),
  time_file(File, LastModified), !,
  debug(rdf_save_any, 'No need to save graph ~w; no updates.', [Graph]).

% 3. 
rdf_save_any(File, Options1):-
  % Derive the RDF output format.
  (   select_option(format(Format), Options1, Options2)
  ->  true
  ;   Options2 = Options1,
      (   file_name_extension(_, Extension, File),
          rdf_file_extension_format(Extension, Format)
      ->  true
      ;   Format = nquads
      )
  ),
  
  % Make sure the directory for the given file name exists.
  % A new file in an existing directory is created on the fly.
  create_file_directory(File),
  
  rdf_save_any(File, Format, Options2),
  
  (   option(silent(true), Options2)
  ->  true
  ;   option(graph(Graph), Options2),
      print_message(informational, rdf_saved(Graph,Format,File))
  ).


% Save to RDF/XML
rdf_save_any(File, rdf_xml, Options):- !,
  rdf_save(File, Options).
% Save to N-Triples.
rdf_save_any(File, ntriples, Options):- !,
  option(graph(Graph), Options, _NoGraph),
  ctriples_write_graph(File, Graph, Options).
% Save to Trig.
rdf_save_any(File, trig, Options):- !,
  rdf_save_trig(File, Options).
% Save to Triples (binary storage format).
rdf_save_any(File, triples, Options):- !,
  option(graph(Graph), Options, user),
  rdf_save_db(File, Graph).
% Save to Turtle.
rdf_save_any(File, turtle, Options1):- !,
  merge_options(
    [
      only_known_prefixes(true),
      tab_distance(0),
      user_prefixes(true)
    ],
    Options1,
    Options2
  ),
  rdf_save_turtle(File, Options2).



% MESSAGE

:- multifile(prolog:message//1).

prolog:message(rdf_saved(Graph,Format,File)) -->
  ['Graph ',Graph,' was saved in ',Format,' serialization to file ',File,'.'].
