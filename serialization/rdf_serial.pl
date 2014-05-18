:- module(
  rdf_serial,
  [
    location_base/2, % +Location:dict
                     % -BaseUri:uri
    rdf_guess_format/5, % +Options:list(nvpair)
                        % +Stream:stream
                        % +Location:dict
                        % -BaseUri:atom
                        % -Format:atom
    rdf_load_any/2, % +Options:list(nvpair)
                    % +Input
    rdf_load_any/3, % +Options:list(nvpair)
                    % +Input
                    % -Pairs:list(pair(atom))
    rdf_save/3, % +Options:list(nvpair)
                % +Graph:atom
                % ?File:atom
    rdf_unload_graph_debug/1 % +Graph:atom
  ]
).

/** <module> RDF serialization

Helper predicates for loading/saving RDF graphs.

Also easily converts between different RDF serializations.

At some point (2014/01/27) I decided that file types indicated by
file extensions are not going to work for LOD,
since most datasets are published in a non-standard way.

@author Wouter Beek
@tbd Writing in the N-triples format is not supported.
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09, 2013/11, 2014/01-2014/04
*/

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(lists)).
:- use_module(library(option)).
% rdf_file_type(xml,   xml    ).
% rdf_file_type(rdf,   xml    ).
% rdf_file_type(rdfs,  xml    ).
% rdf_file_type(owl,   xml    ).
% rdf_file_type(htm,   xhtml  ).
% rdf_file_type(html,  xhtml  ).
% rdf_file_type(xhtml, xhtml  ).
% rdf_file_type(trp,   triples).
% rdf_storage_encoding('', plain).
% url_protocol(file).
:- use_module(library(semweb/rdf_db)).
% rdf_open_hook(http,  ...)
% rdf_open_hook(https, ...)
% rdf_storage_encoding(_, gzip).
% url_protocol(http).
% url_protocol(https).
:- use_module(library(semweb/rdf_http_plugin)).
% rdf_file_type(nt,       ntriples).
% rdf_file_type(ntriples, ntriples).
% rdf_file_type(nq,       nquads  ).
% rdf_file_type(nquads,   nquads  ).
:- use_module(library(semweb/rdf_ntriples)). % Serialization format support.
:- use_module(library(semweb/rdf_turtle_write)).
% rdf_open_decode(gzip, ...)
% rdf_storage_encoding(gz, gzip)
:- use_module(library(semweb/rdf_zlib_plugin)).
% rdf_file_type(html, rdfs).
:- use_module(library(semweb/rdfa)). % Serialization format support.
% rdf_file_type(ttl,  turtle).
% rdf_file_type(n3,   turtle).
% rdf_file_type(trig, trig  ).
:- use_module(library(semweb/turtle)).
:- use_module(library(thread)).

:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(unpack)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_file(rdf_detect)).
:- use_module(rdf_file(rdf_file_db)).
:- use_module(rdf_file(rdf_ntriples_write)).
:- use_module(rdf_file(rdf_serial)).



%! rdf_guess_format(
%!   +Options:list(nvpair),
%!   +Stream:stream,
%!   +Location:dict,
%!   -Base:atom,
%!   -Format:atom
%! ) is det.

rdf_guess_format(O1, Stream, Location, Base, Format):-
  location_base(Location, Base),
  (
    (
      file_name_extension(_, Ext, Base),
      Ext \== '',
      guess_format(Location.put(ext, Ext), DefFormat)
    ;
      guess_format(Location, DefFormat)
    )
  ->
    O2 = [format(DefFormat)|O1]
  ;
    O2 = O1
  ),
  (
    rdf_guess_format(Stream, Format, O2)
  ->
    true
  ;
    print_message(warning, rdf_load_any(no_rdf(Base))),
    fail
  ).


rdf_load_any(O1, Input):-
  rdf_load_any(O1, Input, _).

%! rdf_load_any(
%!   +Option:list(nvpair),
%!   +Input:or([atom,list(atom)]),
%!   -Pairs:list(pair(atom))
%! ) is det.
% Load RDF from a file, a list of files, or a directory.
%
% The following options are supported:
%   * =|format(+Format:oneof([ntriples,turtle,xml]))|=
%   * =|graph(+Graph:atom)|=
%   * =|void(+LoadVoid:boolean)|=

% Loads multiple inputs.
rdf_load_any(O1, Inputs, Pairs):-
  is_list(Inputs), !,
  concurrent_maplist(rdf_load_any_1(O1), Inputs, PairsList),
  append(PairsList, Pairs),
  (
    option(graph(Graph), O1),
    nonvar(Graph)
  ->
    % Copy all triples to the central graph.
    forall(
      member(_-TmpGraph, Pairs),
      call_cleanup(
        rdf_copy(TmpGraph, _, _, _, Graph),
        rdf_unload_graph_debug(TmpGraph)
      )
    )
  ;
    option(loaded(Pairs0), O1)
  ->
    Pairs0 = Pairs
  ;
    true
  ).
% Load all files from a given directory.
rdf_load_any(O1, Dir, Pairs):-
  exists_directory(Dir), !,
  directory_files(
    [file_types([rdf]),include_directories(false),recursive(true)],
    Dir,
    Files
  ),
  rdf_load_any(O1, Files, Pairs).
% A single, non-directory input.
rdf_load_any(O1, Input, Pairs):-
  rdf_load_any_1(O1, Input, Pairs),
  (
    option(loaded(Pairs0), O1)
  ->
    Pairs0 = Pairs
  ;
    true
  ).

rdf_load_any_1(O1, Input, Pairs):-
  % Instantiate the RDF graph name.
  (
    select_option(graph(Graph), O1, O2), !
  ;
    O2 = O1
  ),
  
  findall(
    Base-Graph,
    (
      unpack(Input, Stream, Location),
      call_cleanup(
        (
          location_base(Location, Base),
          load_stream_(Stream, Location, Base, [graph(Graph)|O2])
        ),
        close(Stream)
      ),
      %%%%rdf_load_any_debug(Graph),
      true
    ),
    Pairs
  ).

rdf_load_any_debug(Graph):-
  var(Graph), !.
rdf_load_any_debug(Graph):-
  rdf_statistics(triples_by_graph(Graph,GraphTriples)),
  rdf_statistics(triples(AllTriples)),
  debug(
    mem_triples,
    'PLUS ~:d triples (~:d total)',
    [GraphTriples,AllTriples]
  ).

% Adds a catch/3 around laod_stream_/4.
load_stream(Stream, Location, Base, O1):-
  catch(
    load_stream_(Stream, Location, Base, O1),
    E,
    print_message(warning, E)
  ).

load_stream_(Stream, Location, Base, O1):-
  (
    (
      file_name_extension(_, Ext, Base),
      Ext \== '',
      guess_format(Location.put(ext, Ext), DefFormat)
    ;
      guess_format(Location, DefFormat)
    )
  ->
    O2 = [format(DefFormat)|O1]
  ;
    O2 = O1
  ),
  (
    rdf_guess_format(Stream, Format, O2)
  ->
    print_message(informational, rdf_load_any(rdf(Base, Format))),
    set_stream(Stream, file_name(Base)),
    rdf_load(
      stream(Stream),
      [format(Format),base_uri(Base),register_namespaces(false)|O1]
    )
  ;
    print_message(warning, rdf_load_any(no_rdf(Base))),
    fail
  ).


%! location_base(+Location:atom, -BaseUri:atom) is det.
%  The base URI describes the location from where the data is loaded.

location_base(Location, Base):-
  location_base_base(Location, Base1),
  (
    location_suffix(Location.data, Suffix)
  ->
    atomic_list_concat([Base1, Suffix], /, Base)
  ;
    Base = Base1
  ).

location_base_base(Location, Location.get(url)):- !.
location_base_base(Location, Base):-
  uri_file_name(Base, Location.get(path)), !.
location_base_base(Location, Base):-
  stream_property(Location.get(stream), file_name(FileName)), !,
  (
    uri_is_global(FileName)
  ->
    Base = FileName
  ;
    uri_file_name(Base, FileName)
  ).
location_base_base(_Location, Base):-
  gensym('stream://', Base).

location_suffix([filter(_)|T], Suffix):- !,
  location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix):-
  _{name:data, format:raw} :< Archive, !,
  location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix):-
  (
    location_suffix(T, Suffix0)
  ->
    atomic_list_concat([Archive.name, Suffix0], /, Suffix)
  ;
    Suffix = Archive.name
  ).

guess_format(Location, Format):-
  rdf_content_type(Location.get(content_type), Format), !.
guess_format(Location, Format):-
  rdf_db:rdf_file_type(Location.get(ext), Format).

rdf_content_type('text/rdf',      xml).
rdf_content_type('text/xml',      xml).
rdf_content_type('text/rdf+xml',    xml).
rdf_content_type('application/rdf+xml',    xml).
rdf_content_type('application/x-turtle',  turtle).
rdf_content_type('application/turtle',    turtle).
rdf_content_type('application/trig',    trig).
rdf_content_type('application/n-triples', ntriples).
rdf_content_type('application/n-quads',   nquads).
rdf_content_type('text/turtle',      turtle).
rdf_content_type('text/rdf+n3',      turtle).  % Bit dubious
rdf_content_type('text/html',      html).
rdf_content_type('application/xhtml+xml', xhtml).



%! rdf_save(+Options:list, +Graph:atom, ?File:atom) is det.
% If the file name is not given, then a file name is construed.
% There are two variants here:
%   1. The graph was loaded from a file. Use the same file
%      (if we have write access) to this file.
%   2. Make up the file name based on the given graph name.
%      If the format is specified as well, then this is used to determine
%      the file extension.
%
% The following options are supported:
%   * =|format(+Format:oneof([ntriples,rdf_xml,turtle])|=
%     The serialization format in which the graph is exported.
%   * =|mime(+MIME:oneof(['application/rdf+xml','application/x-turtle','text/plain','text/rdf+n3']))|=
%
% @arg Options A list of name-value pairs.
% @arg File An atomic absolute file name.
%
% @throws =|mime_error(+File:atom, +Type:oneof(['RDF']), MIME:atom)|=

% Derive the file name from the graph.
% This only works if the graph was loaded form file.
rdf_save(O1, Graph, File2):-
  var(File2), !,
  (
    rdf_graph_property(Graph, source(File1))
  ->
    http_path_correction(File1, File2),
    create_file(File2),
    rdf_save(O1, Graph, File2)
  ;
    instantiation_error(File2)
  ).
% Make up the format.
rdf_save(O1, Graph, File):-
  (
    % We do not need to save the graph if
    % (1) the contents of the graph did not change, and
    % (2) the serialization format of the graph did not change.
    %
    % Make sure the contents of the graph were not changed.
    rdf_graph_property(Graph, modified(false)),
    %
    % Make sure the file is the same.
    rdf_graph_property(Graph, source(FromFile1)),
    http_path_correction(FromFile1, FromFile2),
    FromFile2 == File
  ->
    debug(rdf_serial, 'No need to save graph ~w; no updates.', [Graph])
  ;
    select_option(format(Format), O1, O2, turtle),
    rdf_save(O2, Format, Graph, File),
    debug(
      rdf_serial,
      'Graph ~w was saved in ~w serialization to file ~w.',
      [Graph,Format,File]
    )
  ).

% Save to RDF/XML
rdf_save(O1, rdf_xml, Graph, File):- !,
  merge_options([graph(Graph)], O1, O2),
  rdf_save(File, O2).
% Save to N-Triples.
rdf_save(O1, ntriples, Graph, File):- !,
  merge_options([graph(Graph)], O1, O2),
  rdf_ntriples_write(File, O2).
% Save to Triples (binary storage format).
rdf_save(_, triples, Graph, File):- !,
  rdf_save_db(File, Graph).
% Save to Turtle.
rdf_save(O1, turtle, Graph, File):- !,
  merge_options([graph(Graph)], O1, O2),
  rdf_save_canonical_turtle(File, O2).


%! rdf_unload_graph_debug(+Graph:atom) is det.

rdf_unload_graph_debug(Graph):-
  rdf_statistics(triples_by_graph(Graph,GraphTriples)),
  rdf_unload_graph(Graph),
  rdf_statistics(triples(AllTriples)),
  debug(
    mem_triples,
    'MINUS ~:d triples (~:d total)',
    [GraphTriples,AllTriples]
  ).



% MESSAGES

:- multifile(prolog:message//1).

prolog:message(rdf_load_any(rdf(Base, Format))) -->
  [ 'RDF in ~q: ~q'-[Base, Format] ].
prolog:message(rdf_load_any(no_rdf(Base))) -->
  [ 'No RDF in ~q'-[Base] ].

