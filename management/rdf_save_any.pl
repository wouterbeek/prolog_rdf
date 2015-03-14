:- module(
  rdf_save_any,
  [
    rdf_save_any/1, % +Options:list(nvpair)
    rdf_save_any/2 % ?Spec:compound
                   % +Options:list(nvpair)
  ]
).

/** <module> RDF Management: Save in any format

@author Wouter Beek
@version 2014/10-2014/12, 2015/02-2015/03
*/

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(uri)).

:- use_module(plc(io/file_ext)).

:- use_module(plRdf(management/rdf_file_db)).
:- use_module(plRdf(syntax/ctriples/ctriples_write_graph)).

:- predicate_options(rdf_save_any/1, 1, [
  pass_to(rdf_save_any/2, 2)
]).
:- predicate_options(rdf_save_any/2, 2, [
  format(+oneof([ntriples,rdf_xml,trig,triples,turtle])),
  graph(+atom),
  silent(+boolean),
  pass_to(rdf_save_any/3, 3)
]).
:- predicate_options(rdf_save_any/3, 3, [
  compress(+oneof([deflate,gzip])),
  pass_to(ctriples_write_graph/3, 3),
  pass_to(rdf_save/2, 2),
  pass_to(rdf_save_trig/2, 2),
  pass_to(rdf_save_turtle/2, 2)
]).





%! rdf_save_any(+Options:list(nvpair)) is det.

rdf_save_any(Options):-
  rdf_save_any(_, Options).

%! rdf_save_any(?Out, +Options:list(nvpair)) is det.
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

% 1. File specification.
rdf_save_any(file_spec(Spec), Options):- !,
  absolute_file_name(Spec, File, [access(write)]),
  rdf_save_any(file(File), Options).

% 2. Uninstantiated `Out`; come up with a file name.
% File name derived from graph.
% This only works if a graph option is given
% and the denoted graph was loaded from file.
rdf_save_any(file(File), Options):-
  var(File), !,
  (   option(graph(Graph0), Options)
  ->  (   rdf_graph_property(Graph0, source(File0))
      ->  % The given graph is associated with a file; reuse that file.
          uri_file_name(File0, File)
      ;   % The given graph is not associated with a file;
          % try to construct a file name based on the graph name.

          % In case a serialization format is specified,
          % we use the appropriate file extension.
          (   option(format(Format), Options)
          ->  rdf_file_extension_format(Ext, Format),
              file_name_extension(Graph0, Ext, Graph)
          ;   Graph = Graph0
          ),
          absolute_file_name(data(Graph), File, [access(write)])
      ),
      create_file(File),
      rdf_save_any(file(File), Options)
  ;   instantiation_error(File)
  ).

% 3. `Out` instantiated to a file name: No modifications.
rdf_save_any(file(File), Options):-
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

% 4. `Out` instantiated to a file name: There are modifications.
rdf_save_any(file(File), Options1):-
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

  rdf_save_any(file(File), Format, Options2),

  (   option(silent(true), Options2)
  ->  true
  ;   option(graph(Graph), Options2),
      print_message(informational, rdf_saved(Graph,Format,File))
  ).

% 5. Heuristic: unspecified spec is a file.

rdf_save_any(File, Options):-
  rdf_save_any(file(File), Options).



%! rdf_save_any(+Spec:compound, +Format:atom, +Options:list(nvpair)) is det.

rdf_save_any(file(File), Format, Options):- !,
  (   option(compress(Compress), Options)
  ->  setup_call_cleanup(
        gzopen(File, write, Write, [format(Compress)]),
        rdf_save_any(stream(Write), Format, Options),
        close(Write)
      )
  ;   setup_call_cleanup(
        open(File, write, Write),
        rdf_save_any(stream(Write), Format, Options),
        close(Write)
      )
  ).

% Save to RDF/XML
rdf_save_any(stream(Write), rdf_xml, Options):- !,
  rdf_save(Write, Options).
% Save to N-Triples.
rdf_save_any(stream(Write), Format, Options1):-
  (   Format == ntriples
  ->  CFormat = triples
  ;   Format == nquads
  ->  CFormat = quadruples
  ), !,
  option(graph(Graph), Options1, _NoGraph),
  merge_options([format(CFormat)], Options1, Options2),
  ctriples_write_graph(Write, Graph, Options2).
% Save to Trig.
rdf_save_any(stream(Write), trig, Options):- !,
  rdf_save_trig(Write, Options).
% Save to Triples (binary storage format).
rdf_save_any(stream(Write), triples, Options):- !,
  (   option(graph(Graph), Options)
  ->  rdf_save_db(Write, Graph)
  ;   rdf_save_db(Write)
  ).
% Save to Turtle.
rdf_save_any(stream(Write), turtle, Options1):- !,
  merge_options(
    [
      only_known_prefixes(true),
      tab_distance(0),
      user_prefixes(true)
    ],
    Options1,
    Options2
  ),
  rdf_save_turtle(Write, Options2).





% MESSAGE

:- multifile(prolog:message//1).

prolog:message(rdf_saved(Graph,Format,File)) -->
  ['Graph ',Graph,' was saved in ',Format,' serialization to file ',File,'.'].
