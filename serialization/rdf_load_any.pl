:- module(
  rdf_load_any,
  [
    rdf_load_any/2 % +Input
                   % +Options:list(nvpair)
  ]
).

/** <module> RDF: load any

@author Wouter Beek
@author Jan Wielemaker
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09, 2013/11, 2014/01-2014/04, 2014/07, 2014/10
*/

:- use_module(library(error)).
:- use_module(library(http/http_cookie)).
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

:- use_module(generics(db_ext)).
:- use_module(http(http_download)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(open_any)).

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdf_prefixes)).
:- use_module(plRdf_ser(ctriples_write_graph)).
:- use_module(plRdf_ser(rdf_file_db)).
:- use_module(plRdf_ser(rdf_guess_format)).

:- predicate_options(rdf_load_any/2, 2, [
     keep_file(+boolean),
     pass_to(open_any/3, 3),
     pass_to(rdf_load_from_stream/2, 2)
   ]).
:- predicate_options(rdf_load_from_stream/2, 2, [
     graph(+atom),
     graphs(-list(pair(atom))),
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



%! rdf_load_any(+Input, +Option:list(nvpair)) is det.
% Load RDF from a stream, a URL, a file, a list of files, or a file directory.
%
% Input can be one of the following:
%   - List
%   - file/1
%   - prefix/1
%   - url/1
%   - uri_components/5
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
%   * =|reduced_locations(+Use:boolean)|=
%     Whether reduced locations are used of not.
%     See [rdf_prefixes].
%     Default: `false`.
%   * =|void(+Load:boolean)|=
%     Whether the loaded data should be recursively closed under
%     VoID descriptions that appear in that data.
%     Default: `false`.

% 1. Load the URI denoted by a registered RDF prefix.
rdf_load_any(prefix(Prefix), Options):-
  rdf_current_prefix(Prefix, Uri), !,
  rdf_load_any(uri(Uri), Options).

% 2. Load from a URL with reduced location.
rdf_load_any(uri(Uri), Options1):-
  select_option(reduced_locations(true), Options1, Options2),
  rdf_reduced_location(Uri, ReducedUri), !,
  rdf_load_any(uri(ReducedUri), Options2).

% 3. Reuse the versatile open_any/3.
rdf_load_any(Input, Options1):-
  rdf_extra_headers(ExtraHeaders),
  merge_options([metadata(Metadata)], Options1, Options2),
  merge_options(Options2, ExtraHeaders, Options3),
  
  % Load all individual RDF graphs.
  findall(
    Base-Graph,
    (
      open_any(Input, Out, Options3),
      call_cleanup(
        (
          metadata_to_base(Metadata, Base),
          rdf_load_from_stream(Out, Metadata, Base, Options),
          option(graph(Graph), Options, _NoGraph)
        ),
        close(Out)
      )
    ),
    Pairs
  ),

  % Return the graphs option.
  (   option(graphs(Pairs0), Options)
  ->  Pairs0 = Pairs
  ;   true
  ).


rdf_load_from_stream(Read, Metadata, Base, Options1):-
  % Guess the RDF serialization format.
  ignore(file_name_extension(_, FileExtension, Base)),
  ignore(ContentType = Metadata.get(content_type)),
  rdf_guess_format(Read, FileExtension, ContentType, Format),

  % DEB
  print_message(informational, rdf_load_any(rdf(Base,Format))),

  % Collect options: base URI, RDF serialization format, XML namespaces.
  set_stream(Read, file_name(Base)),
  merge_options(
    [base_uri(Base),format(Format),register_namespaces(false)],
    Options1,
    Options2
  ),

  % Add options that are specific to the RDFa serialization format.
  (   Format == rdfa
  ->  merge_options([max_errors(-1),syntax(style)], Options2, Options3)
  ;   Options3 = Options2
  ),

  % The actual loading of the RDF data.
  catch(
    rdf_load(stream(Read), Options3),
    Exception,
    print_message(warning, Exception)
  ).

/*
% Load all files from a given directory.
rdf_load_any(file(Dir), Options):-
  exists_directory(Dir), !,
  directory_files(
    [file_types([rdf]),include_directories(false),recursive(true)],
    Dir,
    Files
  ),
  maplist(file_term, Files, FileTerms),
  rdf_load_any(FileTerms, Options).
*/


%! rdf_extra_headers(-Headers:list(nvpair)) is det.

rdf_extra_headers([
  cert_verify_hook(ssl_verify),
  request_header('Accept'=AcceptValue)
]):-
  rdf_accept_header_value(AcceptValue).


%! metadata_to_base(+Metadata:dict, -Base:uri) is det.
%  The base URI describes the location from where the data is loaded.

metadata_to_base(Metadata, Base):-
  metadata_to_base0(Metadata, Base0),
  (   location_suffix(Metadata.data, Suffix)
  ->  atomic_list_concat([Base0,Suffix], '/', Base)
  ;   Base = Base0
  ).


%! metadata_to_base0(+Metadata:dict, -Base0:atom) is det.

metadata_to_base0(Metadata, Metadata.get(url)):- !.
metadata_to_base0(Metadata, Base):-
  uri_file_name(Base, Metadata.get(path)), !.
metadata_to_base0(Metadata, Base):-
  stream_property(Metadata.get(stream), file_name(FileName)), !,
  (   uri_is_global(FileName)
  ->  Base = FileName
  ;   uri_file_name(Base, FileName)
  ).
metadata_to_base0(_Location, Base):-
  gensym('stream://', Base).


%! location_suffix(+EntryMetadata, -Suffix:atom) is det.

location_suffix([filter(_)|T], Suffix):- !,
  location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix):-
  _{name:data, format:raw} :< Archive, !,
  location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix):-
  (   location_suffix(T, Suffix0)
  ->  atomic_list_concat([Archive.name, Suffix0], /, Suffix)
  ;   Suffix = Archive.name
  ).



% Messages

:- multifile(prolog:message//1).

prolog:message(rdf_load_any(rdf(Base,Format))) -->
  ['RDF in ~q: ~q'-[Base,Format]].
prolog:message(rdf_load_any(no_rdf(Base))) -->
  ['No RDF in ~q'-[Base]].

