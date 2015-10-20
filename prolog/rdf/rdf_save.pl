:- module(
  rdf_save,
  [
    rdf_save_any/1, % ?Output
    rdf_save_any/2, % ?Output
                    % +Options:list(compound)
    rdf_write_to_graph/2, % +Output
                          % :Goal_1
    rdf_write_to_graph/3 % +Output
                         % :Goal_1
                         % +Options:list(compound)
  ]
).

/** <module> RDF save

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(ctriples/ctriples_write_graph)).
:- use_module(library(debug)).
:- use_module(library(iostream)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(rdf/rdf_file)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(uri)).

:- meta_predicate(rdf_write_to_graph(+,1)).
:- meta_predicate(rdf_write_to_graph(+,1,+)).
:- meta_predicate(rdf_write_to_graph0(1,+,+,+)).

:- predicate_options(rdf_save_any/2, 2, [
     format(+oneof([cquads,ctriples,nquads,ntriples,trig,triples,turtle,xml])),
     graph(+atom),
     pass_to(rdf_save_any0/4, 2),
     pass_to(rdf_stream_write/3,3)
   ]).
:- predicate_options(rdf_save_any0/4, 2, [
     pass_to(ctriples_write_graph/3, 3),
     pass_to(rdf_save/2, 2),
     pass_to(rdf_save_trig/2, 2),
     pass_to(rdf_save_turtle/2, 2)
   ]).
:- predicate_options(rdf_write_to_graph/3, 3, [
     pass_to(rdf_stream_write/3, 3),
     pass_to(rdf_write_to_graph0/3, 2)
   ]).
:- predicate_options(rdf_write_to_graph0/3, 2, [
     pass_to(rdf_save_any/2, 2)
   ]).





%! rdf_save_any(+Output) is det.
% Wrapper around rdf_save_any/2 with default options.

rdf_save_any(Out):-
  rdf_save_any(Out, []).


%! rdf_save_any(+Output, +Options:list(compound)) is det.
% The following options are supported:
%   * format(+oneof([cquads,ctriples,nquads,ntriples,trig,triples,turtle,xml]))
%   * graph(+atom)

% The file name can be derived from the graph.
rdf_save_any(Out, Opts):-
  var(Out),
  option(graph(G0), Opts),
  rdf_graph_property(G0, source(File0)), !,
  uri_file_name(File0, File),
  rdf_save_any(File, Opts).
% A new file name is created based on graph and format.
rdf_save_any(Out, Opts):-
  var(Out), !,
  option(graph(Base), Opts, out),
  % In case a serialization format is specified,
  % we use the appropriate file extension.
  (   option(format(Format), Opts)
  ->  rdf_file_extension(Ext, Format),
      file_name_extension(Base, Ext, Local)
  ;   Local = Base
  ),
  absolute_file_name(Local, File, [access(write)]),
  rdf_save_any(File, Opts).
% We do not need to save the graph if:
%   1. the contents of the graph did not change, and
%   2. the serialization format of the graph did not change, and
%   3. the output file is the same.
rdf_save_any(File, Opts):-
  is_absolute_file_name(File),
  option(graph(G), Opts),
  
  % The graph was not modified after the last save operation.
  rdf_graph_property(G, modified(false)),
  
  % The given file is the source of the given graph.
  rdf_graph_property(G, source(File0)),
  uri_file_name(File0, File),
  
  % The file was not modified after the graph was loaded.
  rdf_graph_property(G, source_last_modified(LMod)),
  exists_file(File),
  time_file(File, LMod), !,
  debug(rdf(save), "No need to save graph ~w; no updates.", [G]).
% "If you know how to do it and you can do it... DO IT!"
rdf_save_any(Out, Opts):-
  % Determine the RDF output format:
  %   1. By option.
  %   2. By the file name extension.
  %   3. Default to `nquads'.
  (   option(format(Format), Opts)
  ->  true
  ;   is_absolute_file_name(Out),
      file_name_extension(_, Ext, Out),
      rdf_file_extension(Ext, Format)
  ->  true
  ;   Format = cquads
  ),

  % Make sure the directory exists.
  (is_absolute_file_name(Out) -> create_file_directory(Out) ; true),
  
  rdf_stream_write(Out, rdf_save_any0(Format, Opts), Opts).


% XML/RDF.
rdf_save_any0(xml, Opts, _, Write):- !,
  rdf_save(Write, Opts).
% N-Triples.
rdf_save_any0(ntriples, Opts, M, Write):- !,
  rdf_save_any0(ctriples, Opts, M, Write).
% N-Quads.
rdf_save_any0(nquads, Opts, M, Write):- !,
  rdf_save_any0(cquads, Opts, M, Write).
% C-Triples / C-Quads
rdf_save_any0(Format, Opts0, _, Write):-
  (   Format == ctriples
  ->  CFormat = triples
  ;   Format == cquads
  ->  CFormat = quadruples
  ), !,
  option(graph(G), Opts0, _NO_GRAPH),
  % Overwrite the format option.
  merge_options([format(CFormat)], Opts0, Opts),
  ctriples_write_graph(Write, G, Opts).
% TriG.
rdf_save_any0(trig, Opts, _, Write):- !,
  rdf_save_trig(Write, Opts).
% Binary storage format.
rdf_save_any0(triples, Opts, _, Write):- !,
  (   option(graph(G), Opts)
  ->  rdf_save_db(Write, G)
  ;   rdf_save_db(Write)
  ).
% Turtle.
rdf_save_any0(turtle, Opts0, _, Write):- !,
  merge_options(
    [only_known_prefixes(true),tab_distance(0),user_prefixes(true)],
    Opts0,
    Opts
  ),
  rdf_save_turtle(Write, Opts).



%! rdf_write_to_graph(+Out, :Goal_1) is det.
% Wrapper around rdf_write_to_graph/3 with default options.

rdf_write_to_graph(Out, Goal_1):-
  rdf_write_to_graph(Out, Goal_1, []).


%! rdf_write_to_graph(+Out, :Goal_1, +Options:list(compound)) is det.
% Writes results of Goal_1 asserted in its additional Graph argument
% to Out.
%
% The following options are supported:
%   * compress(+oneof([deflate,gzip,none]))
%     Whether, and if so which, compression is used.
%     By default no compression is used.
%   * format(+oneof([cquads,ctriples,nquads,ntriples,trig,triples,turtle,xml]))
%     The output format that is used for writing.
%     Default is `cquads`.

rdf_write_to_graph(Out, Goal_1, Opts):-
  rdf_stream_write(Out, rdf_write_to_graph0(Goal_1, Opts), Opts).

rdf_write_to_graph0(Goal_1, Opts0, _, Write):-
  setup_call_cleanup(
    rdf_tmp_graph(G),
    (
      call(Goal_1, G),
      merge_options([graph(G)], Opts0, Opts),
      rdf_save_any(stream(Write), Opts)
    ),
    rdf_unload_graph(G)
  ).
