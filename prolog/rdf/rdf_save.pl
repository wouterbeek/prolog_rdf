:- module(
  rdf_save,
  [
    rdf_save_file/1, % ?Sink
    rdf_save_file/2, % ?Sink
                     % +Options:list(compound)
    rdf_write_to_graph/2, % +Sink, :Goal_1
    rdf_write_to_graph/3 % +Sink
                         % :Goal_1
                         % +Options:list(compound)
  ]
).

/** <module> RDF: Save data

@author Wouter Beek
@version 2015/08, 2015/10-2016/01
*/

:- use_module(library(debug)).
:- use_module(library(iostream)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(rdf/rdf_file)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdf_db), [rdf_save/2 as rdf_save_xmlrdf]).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(simple/write_SimpleRDF)).
:- use_module(library(uri)).

:- meta_predicate(rdf_write_to_graph(+,1)).
:- meta_predicate(rdf_write_to_graph(+,1,+)).
:- meta_predicate(rdf_write_to_graph(1,+,+,+)).

:- rdf_meta(rdf_save_file(+,t)).

:- predicate_options(rdf_save_file/2, 2, [
     format(+oneof([simpleQuads,simpleTriples,nquads,ntriples,trig,triples,turtle,xml])),
     graph(+iri),
     pass_to(rdf_save_to_stream/4, 2),
     pass_to(rdf_write_to_stream/3, 3)
   ]).
:- predicate_options(rdf_save_to_stream/4, 2, [
     pass_to(write_simple_graph/2, 2),
     pass_to(rdf_save_trig/2, 2),
     pass_to(rdf_save_turtle/2, 2),
     pass_to(rdf_save_xmlrdf/2, 2)
   ]).
:- predicate_options(rdf_write_to_graph/3, 3, [
     pass_to(rdf_write_to_stream/3, 3),
     pass_to(rdf_write_to_graph/4, 2)
   ]).
:- predicate_options(rdf_write_to_graph/4, 2, [
     pass_to(rdf_save_file/2, 2)
   ]).





%! rdf_save_file(+Sink) is det.
% Wrapper around rdf_save_file/2 with default options.

rdf_save_file(Out):-
  rdf_save_file(Out, []).


%! rdf_save_file(+Sink, +Options:list(compound)) is det.
% The following options are supported:
%   * format(+oneof([simpleQuads,simpleTriples,nquads,ntriples,trig,triples,turtle,xml]))
%   * graph(+iri)

% The file name can be derived from the graph.
rdf_save_file(Out, Opts):-
  var(Out),
  option(graph(G0), Opts),
  rdf_graph_get_property(G0, source(File0)), !,
  uri_file_name(File0, File),
  rdf_save_file(File, Opts).
% A new file name is created based on graph and format.
rdf_save_file(Out, Opts):-
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
  rdf_save_file(File, Opts).
% We do not need to save the graph if:
%   1. the contents of the graph did not change, and
%   2. the serialization format of the graph did not change, and
%   3. the output file is the same.
rdf_save_file(File, Opts):-
  is_absolute_file_name(File),
  option(graph(G), Opts),
  
  % The graph was not modified after the last save operation.
  rdf_graph_get_property(G, modified(false)),
  
  % The given file is the source of the given graph.
  rdf_graph_get_property(G, source(File0)),
  uri_file_name(File0, File),
  
  % The file was not modified after the graph was loaded.
  rdf_graph_get_property(G, source_last_modified(LMod)),
  exists_file(File),
  time_file(File, LMod), !,
  debug(rdf(save), "No need to save graph ~w; no updates.", [G]).
% "If you know how to do it and you can do it... DO IT!"
rdf_save_file(Out, Opts):-
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
  ;   Format = simpleQuads
  ),

  % Make sure the directory exists.
  (is_absolute_file_name(Out) -> create_file_directory(Out) ; true),
  
  rdf_write_to_stream(Out, rdf_save_to_stream(Format, Opts), Opts).


%! rdf_save_to_stream(
%!   +Format:oneof([simpleQuads,simpleTriples,nquads,ntriples,trig,triples,turtle,xml]),
%!   +Options:list(comound),
%!   +Metadata:dict,
%!   +Write:stream
%! ) is det.

% XML/RDF.
rdf_save_to_stream(xml, Opts, _, Write):- !,
  rdf_save_xmlrdf(Write, Opts).
% N-Triples.
rdf_save_to_stream(ntriples, Opts, M, Write):- !,
  rdf_save_to_stream(simpleTriples, Opts, M, Write).
% N-Quads.
rdf_save_to_stream(nquads, Opts, M, Write):- !,
  rdf_save_to_stream(simpleQuads, Opts, M, Write).
% C-Triples / C-Quads
rdf_save_to_stream(Format, Opts0, _, Write):-
  (   Format == simpleTriples
  ->  SimpleFormat = triples
  ;   Format == simpleQuads
  ->  SimpleFormat = quadruples
  ), !,
  option(graph(G), Opts0, _NO_GRAPH),
  % Overwrite the format option.
  merge_options([format(SimpleFormat)], Opts0, Opts),
  with_output_to(Write, write_simple_graph(G, Opts)).
% TriG.
rdf_save_to_stream(trig, Opts, _, Write):- !,
  rdf_save_trig(Write, Opts).
% Turtle.
rdf_save_to_stream(turtle, Opts0, _, Write):- !,
  merge_options(
    [
      a(true),
      align_prefixes(true),
      comment(true),
      expand(rdf_petty_min),
      group(true),
      indent(4),
      only_known_prefixes(true),
      subject_white_lines(1),
      tab_distance(0),
      user_prefixes(true)
    ],
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
%   * format(+oneof([simpleQuads,simpleTriples,nquads,ntriples,trig,triples,turtle,xml]))
%     The output format that is used for writing.
%     Default is `simpleQuads`.

rdf_write_to_graph(Out, Goal_1, Opts):-
  rdf_write_to_stream(Out, rdf_write_to_graph(Goal_1, Opts), Opts).

rdf_write_to_graph(Goal_1, Opts1, _, Write):-
  setup_call_cleanup(
    rdf_tmp_graph(G),
    (
      call(Goal_1, G),
      merge_options([graph(G)], Opts1, Opts2),
      rdf_save_file(stream(Write), Opts2)
    ),
    rdf_unload_graph(G)
  ).
