:- module(
  rdf_serial,
  [
    rdf_load_any/2, % +Input
                    % +Options:list(nvpair)
    rdf_save_any/2 % ?File:atom
                   % +Options:list(nvpair)
  ]
).

/** <module> RDF serialization

Helper predicates for loading/saving RDF graphs.

Also easily converts between different RDF serializations.

@author Wouter Beek
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09, 2013/11, 2014/01-2014/04, 2014/07
*/

:- use_module(library(error)).
:- use_module(library(http/http_cookie)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(predicate_options)). % Declarations
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

:- use_module(generics(db_ext)).
:- use_module(http(http_download)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(unpack)).

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdf_prefixes)).
:- use_module(plRdf_ser(rdf_detect)).
:- use_module(plRdf_ser(ctriples_write)).

:- predicate_options(rdf_load_any/2, 2, [
     keep_file(+boolean),
     number_of_threads(+positive_integer),
     pass_to(rdf_load_any0/2, 2)
   ]).
:- predicate_options(rdf_load_any0/2, 2, [
     graph(+atom),
     pairs(-list),
     pass_to(rdf_load_stream/4, 4)
   ]).
:- predicate_options(rdf_load_stream/4, 4, [
     pass_to(rdf_load/2, 2)
   ]).

:- initialization(assert_rdf_file_types).
assert_rdf_file_types:-
  forall(
    rdf_db:rdf_file_type(FileExtension, FileType),
    (
      db_add_novel(user:prolog_file_type(FileExtension, FileType)),
      db_add_novel(user:prolog_file_type(FileExtension, rdf))
    )
  ).



%! rdf_load_any(+Input:or([atom,list(atom)]), +Option:list(nvpair)) is det.
% Load RDF from a stream, a URL, a file, a list of files, or a file directory.
%
% The following options are supported:
%   * =|format(+Format:oneof([ntriples,turtle,xml]))|=
%     The RDF serialization that has to be used for parsing.
%     Default: @tbd
%   * =|graph(+Graph:atom)|=
%     The name of the RDF graph in which the parsed data is loaded.
%     Default: `user`.
%   * =|keep_file(+Keep:boolean)|=
%     If the given input is a URL, the remote document is first
%     downloaded, and then kept available, locally.
%     Default: `false`.
%   * =|number_of_threads(+NumberOfThreads:positive_integer)|=
%     The number of threads that are used for loading concurrent jobs.
%     Default: the value of Prolog flag `cpu_count`.
%   * =|void(+LoadVoid:boolean)|=
%     Whether the loaded data should be recursively closed under
%     VoID descriptions that appear in that data.
%     Default: `false`.

% Load multiple inputs.
rdf_load_any(Inputs, Options):-
  is_list(Inputs), !,
  findall(
    rdf_load_any0(Input, Options),
    member(Input, Inputs),
    Goals
  ),
  current_prolog_flag(cpu_count, DefaultNumberOfThreads),
  option(number_of_threads(NumberOfThreads), Options, DefaultNumberOfThreads),
  concurrent(NumberOfThreads, Goals, []).
% Load all files from a given directory.
rdf_load_any(Dir, Options):-
  exists_directory(Dir), !,
  directory_files(
    [file_types([rdf]),include_directories(false),recursive(true)],
    Dir,
    Files
  ),
  rdf_load_any(Files, Options).
% A single, non-directory input: keep a file around.
rdf_load_any(Url1, Options1):-
  select_option(keep_file(true), Options1, Options2, false),
  rdf_reduced_location(Url1, Url2), !,
  download_to_file(Url2, File, [freshness_lifetime(8640000)]),
  rdf_load_any(File, Options2).
% A single, non-directory input: do not keep a file around.
rdf_load_any(Input, Options):-
  rdf_load_any0(Input, Options).

rdf_load_any0(Input, Options1):-
  % Instantiate the RDF graph name.
  (
    select_option(graph(Graph), Options1, Options2), !
  ;
    Options2 = Options1
  ),

  % Load all individual RDF graphs.
  findall(
    Base-Graph,
    (
      unpack(Input, Read, Location),
      call_cleanup(
        (
          location_base(Location, Base),
          rdf_load_stream(Read, Location, Base, [graph(Graph)|Options2])
        ),
        close(Read)
      )
    ),
    Pairs
  ),

  % Return the pairs option.
  (
    option(pairs(Pairs0), Options2)
  ->
    Pairs0 = Pairs
  ;
    option(graph(Graph), Options1),
    Pairs = [_-Graph]
  ->
    true
  ;
    true
  ).

rdf_load_stream(Read, Location, Base, Options1):-
  % Guess the RDF serialization format.
  ignore(file_name_extension(_, FileExtension, Base)),
  ignore(ContentType = Location.get(content_type)),
  rdf_guess_format(Read, FileExtension, ContentType, Format),

  % DEB
  print_message(informational, rdf_load_any(rdf(Base,Format))),
  
  % Collect options: base URI, RDF serialization format, XML namespaces.
  set_stream(Read, file_name(Base)),
  merge_options(
    [base_uri(Base),format(Format),register_namespaces(false)],
    Options1,
    Options3
  ),
  
  % Add options that are specific to the RDFa serialization format.
  (
    Format == rdfa
  ->
    merge_options([max_errors(-1),syntax(style)], Options2, Options3)
  ;
    Options3 = Options2
  ),
  
  % The actual loading of the RDF data.
  catch(
    rdf_load(stream(Read), Options3),
    Exception,
    print_message(warning, Exception)
  ).


%! rdf_save_any(?File:atom, +Options:list(nvpair)) is det.
% If the file name is not given, then a file name is construed.
% There are two variants here:
%   1. The graph was loaded from a file. Use the same file
%      (if we have write access) to this file.
%   2. Make up the file name based on the given graph name.
%      If the format is specified as well, then this is used to determine
%      the file extension.

% Derive the file name from the graph.
% This only works if the graph was loaded form file.
rdf_save_any(File2, Options):-
  var(File2), !,
  (
    option(graph(Graph), Options),
    rdf_graph_property(Graph, source(File1))
  ->
    http_path_correction(File1, File2),
    create_file(File2),
    rdf_save_any(File2, Options)
  ;
    instantiation_error(File2)
  ).
% Make up the format.
rdf_save_any(File, Options1):-
  (
    % We do not need to save the graph if
    % (1) the contents of the graph did not change, and
    % (2) the serialization format of the graph did not change.
    %
    % Make sure the contents of the graph were not changed.
    option(graph(Graph), Options1),
    rdf_graph_property(Graph, modified(false)),

    % Make sure the file is the same.
    rdf_graph_property(Graph, source(FromFile1)),
    http_path_correction(FromFile1, FromFile2),
    FromFile2 == File,

    % The file was not modified after the graph was loaded.
    rdf_graph_property(Graph, source_last_modified(LastModified)),
    exists_file(File),
    time_file(File, LastModified)
  ->
    debug(rdf_serial, 'No need to save graph ~w; no updates.', [Graph])
  ;
    select_option(graph(Graph), Options1, Options2, _NoGraph),
    select_option(format(Format), Options2, Options3, turtle),
    
    % Make sure the directory for the given file name exists.
    % A new file in an existing directory is created on the fly.
    create_file_directory(File),
    
    rdf_save_any(Options3, Format, Graph, File),
    
    debug(
      rdf_serial,
      'Graph ~w was saved in ~w serialization to file ~w.',
      [Graph,Format,File]
    )
  ).

% Save to RDF/XML
rdf_save_any(Options1, rdf_xml, Graph, File):- !,
  merge_options([graph(Graph)], Options1, Options2),
  rdf_save(File, Options2).
% Save to N-Triples.
rdf_save_any(Options1, ntriples, Graph, File):- !,
  merge_options([graph(Graph)], Options1, Options2),
  ctriples_write(File, Options2).
% Save to Trig.
rdf_save_any(Options1, trig, Graph, File):- !,
  merge_options([graph(Graph)], Options1, Options2),
  rdf_save_trig(File, Options2).
% Save to Triples (binary storage format).
rdf_save_any(_, triples, Graph, File):- !,
  rdf_save_db(File, Graph).
% Save to Turtle.
rdf_save_any(Options1, turtle, Graph, File):- !,
  merge_options([graph(Graph)], Options1, Options2),
  rdf_save_canonical_turtle(File, Options2).



% Helpers

%! location_base(+Location:dict, -Base:uri) is det.
%  The base URI describes the location from where the data is loaded.

location_base(Location, Base):-
  location_base_base(Location, Base1),
  (
    location_suffix(Location.data, Suffix)
  ->
    atomic_list_concat([Base1,Suffix], '/', Base)
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



% Messages

:- multifile(prolog:message//1).

prolog:message(rdf_load_any(rdf(Base,Format))) -->
  ['RDF in ~q: ~q'-[Base,Format]].
prolog:message(rdf_load_any(no_rdf(Base))) -->
  ['No RDF in ~q'-[Base]].

