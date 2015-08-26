:- module(
  rdf_save,
  [
    rdf_save_any/2 % ?Spec
                   % +Options:list(compound)
  ]
).

/** <module> RDF save

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(ctriples/ctriples_write_graph)).
:- use_module(library(debug)).
:- use_module(library(iostream)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(rdf/rdf_file)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(uri)).

:- predicate_options(rdf_save_any/2, 2, [
     format(+oneof([cquads,ctriples,nquads,ntriples,trig,triples,turtle,xml])),
     graph(+atom),
     pass_to(rdf_save_any0/3, 2),
     pass_to(rdf_stream_write/3,3)
   ]).
:- predicate_options(rdf_save_any0/3, 2, [
     pass_to(ctriples_write_graph/3, 3),
     pass_to(rdf_save/2, 2),
     pass_to(rdf_save_trig/2, 2),
     pass_to(rdf_save_turtle/2, 2)
   ]).





%! rdf_save_any(+Spec) is det.
% Wrapper around rdf_save_any/2 with default options.

rdf_save_any(Spec):-
  rdf_save_any(Spec, []).

% The file name can be derived from the graph.
rdf_save_any(Spec, Opts):-
  var(Spec),
  option(graph(G0), Opts),
  rdf_graph_property(G0, source(File0)), !,
  uri_file_name(File0, File),
  rdf_save_any(File, Opts).
% A new file name is created.
rdf_save_any(Spec, Opts):-
  var(Spec), !,
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
% We do not need to save the graph if
% (1) the contents of the graph did not change, and
% (2) the serialization format of the graph did not change, and
% (3) the output file is the same.
rdf_save_any(File, Opts):-
  is_absolute_file_name(File),
  option(graph(G), Opts),
  
  % The graph was not modified after the last save operation.
  rdf_graph_property(G, modified(false)),
  
  % The given file is the source of the given graph.
  rdf_graph_property(G, source(File0)),
  uri_file_name(File0, File),
  
  % The file was not modified after the graph was loaded.
  rdf_graph_property(G, source_last_modified(LastModified)),
  exists_file(File),
  dateTime_file(File, LastModified), !,
  debug(rdf_save, 'No need to save graph ~w; no updates.', [G]).
% If you know how to do it and you can do it... DO IT!
rdf_save_any(Spec, Opts):-
  % Derive the RDF output format.
  (   option(format(Format), Opts)
  ->  true
  ;   is_absolute_file_name(Spec),
      file_name_extension(_, Ext, Spec),
      rdf_file_extension(Ext, Format)
  ->  true
  ;   Format = nquads
  ),

  % Make sure the directory exists.
  (   is_absolute_file_name(Spec)
  ->  create_file_directory(Spec)
  ;   true
  ),
  
  rdf_stream_write(Spec, rdf_save_any0(Format, Opts), Opts).

% XML/RDF.
rdf_save_any0(xml, Opts, Write):- !,
  rdf_save(Write, Opts).
% N-Triples.
rdf_save_any0(ntriples, Opts, Write):- !,
  rdf_save_any0(ctriples, Opts, Write).
% N-Quads.
rdf_save_any0(nquads, Opts, Write):- !,
  rdf_save_any0(cquads, Opts, Write).
% C-Triples / C-Quads
rdf_save_any0(Format, Opts0, Write):-
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
rdf_save_any0(trig, Opts, Write):- !,
  rdf_save_trig(Write, Opts).
% Binary storage format.
rdf_save_any0(triples, Opts, Write):- !,
  (   option(graph(G), Opts)
  ->  rdf_save_db(Write, G)
  ;   rdf_save_db(Write)
  ).
% Turtle.
rdf_save_any0(turtle, Opts0, Write):- !,
  merge_options(
    [only_known_prefixes(true),tab_distance(0),user_prefixes(true)],
    Opts0,
    Opts
  ),
  rdf_save_turtle(Write, Opts).
