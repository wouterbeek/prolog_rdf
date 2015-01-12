:- module(
  rdf_load_any,
  [
    rdf_load_any/1, % +In
    rdf_load_any/2, % +In
                    % +Options:list(nvpair)
    rdf_load_any/3 % +In
                   % -Metadata:dict
                   % +Options:list(nvpair)
  ]
).

/** <module> RDF: load any

@author Wouter Beek
@author Jan Wielemaker
@version 2012/01, 2012/03, 2012/09, 2012/11, 2013/01-2013/06,
         2013/08-2013/09, 2013/11, 2014/01-2014/04, 2014/07, 2014/10, 2014/12
*/

:- use_module(library(aggregate)).
:- use_module(library(http/http_ssl_plugin)).
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
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
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
% rdf_open_decode(gzip, ...)
% rdf_storage_encoding(gz, gzip)
:- use_module(library(semweb/rdf_zlib_plugin)).
% rdf_file_type(html, rdfs).
:- use_module(library(semweb/rdfa)). % Serialization format support.
% rdf_file_type(ttl,  turtle).
% rdf_file_type(n3,   turtle).
% rdf_file_type(trig, trig  ).
:- use_module(library(semweb/turtle)). % Serialization format support.

:- use_module(generics(db_ext)).
:- use_module(os(open_any)).

:- use_module(plDcg(dcg_generics)).
:- use_module(plDcg(dcg_pl_term)).

:- use_module(plRdf(management/rdf_file_db)).
:- use_module(plRdf(management/rdf_guess_format)).
:- use_module(plRdf(management/rdf_prefixes)).

:- predicate_options(rdf_load_any/2, 2, [
     pass_to(rdf_load_any/3, 3)
   ]).
:- predicate_options(rdf_load_any/3, 3, [
     pass_to(open_any/4, 4),
     pass_to(rdf_load_from_stream_nondet/3, 3)
   ]).
:- predicate_options(rdf_load_from_stream_det/4, 4, [
     pass_to(rdf_load/2, 2)
   ]).
:- predicate_options(rdf_load_from_stream_nondet/3, 3, [
     silent(+boolean),
     pass_to(rdf_load_from_stream_det/4, 4)
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





%! rdf_load_any(+In) is det.

rdf_load_any(In):-
  rdf_load_any(In, []).


%! rdf_load_any(+In, +Option:list(nvpair)) is det.

rdf_load_any(In, Options):-
  rdf_load_any(In, _, Options).


%! rdf_load_any(+In, -Metadata:dict, +Option:list(nvpair)) is det.
% Load RDF from a stream, a URL, a file, a list of files, or a file directory.
%
% In can be one of the following:
%   - List
%   - file/1
%   - prefix/1
%   - url/1
%   - uri_components/5
%
% The following options are supported:
%   - `format(+Format:oneof([ntriples,turtle,xml]))`
%     The RDF serialization that has to be used for parsing.
%     Default: @tbd
%   - `graph(+Graph:atom)`
%     The name of the RDF graph in which the parsed data is loaded.
%     Default: `user`.
%   - `keep_file(+Keep:boolean)`
%     If the given input is a URL, the remote document is first
%     downloaded, and then kept available, locally.
%     Default: `false`.
%   - `reduced_locations(+Use:boolean)`
%     Whether reduced locations are used of not.
%     See [rdf_prefixes].
%     Default: `false`.
%   - `silent(+boolean)`
%     Whether informational messages about loaded content are shown.
%     Default: `true`.
%   - `void(+Load:boolean)`
%     Whether the loaded data should be recursively closed under
%     VoID descriptions that appear in that data.
%     Default: `false`.

/*
% 4. Load all files from a given directory.
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

% 1. Load the URI denoted by a registered RDF prefix.
rdf_load_any(prefix(Prefix), M, Options):-
  rdf_current_prefix(Prefix, Uri), !,
  rdf_load_any(uri(Uri), M, Options).

% 2. Load from a URL with reduced location.
rdf_load_any(uri(Uri), M, Options1):-
  select_option(reduced_locations(true), Options1, Options2),
  rdf_reduced_location(Uri, ReducedUri), !,
  rdf_load_any(uri(ReducedUri), M, Options2).

% 3. Reuse the versatile open_any/4.
rdf_load_any(In, json{entries:EntryMetadatas}, Options1):-
  rdf_extra_headers(ExtraHeaders),
  merge_options(Options1, ExtraHeaders, Options2),
  findall(
    EntryMetadata,
    rdf_load_from_stream_nondet(In, EntryMetadata, Options2),
    EntryMetadatas
  ).


%! rdf_load_from_stream_nondet(
%!   +In:stream,
%!   -StreamMetadata:dict,
%!   +Options:list(nvpair)
%! ) is det.

rdf_load_from_stream_nondet(In, StreamMetadata, Options):-
  % NONDET: iterates over archive substreams recursively.
  open_any(In, SubIn, OpenMetadata, Options),
  call_cleanup(
    rdf_load_from_stream_det(SubIn, OpenMetadata, RdfMetadata, Options),
    close_any(SubIn, CloseMetadata)
  ),
  StreamMetadata = RdfMetadata.put(json{stream:CloseMetadata}),

  % Allow informational messages to be skipped in silent mode.
  (   option(silent(true), Options)
  ->  true
  ;   print_message(informational, rdf_load_any(StreamMetadata))
  ).



%! rdf_load_from_stream_det(
%!   +In:stream,
%!   +OldMetadata:dict,
%!   -NewMetadata:dict,
%!   +Options:list(nvpair)
%! ) is det.

rdf_load_from_stream_det(In, Metadata1, Metadata2, Options1):-
  % Return the file name extension as metadata.
  metadata_to_base(Metadata1, Base),
  ignore(file_name_extension(_, FileExtension, Base)),

  % Guess the RDF serialization format based on
  % the HTTP Content-Type header value and the file name extension.
  ignore(metadata_content_type(Metadata1, ContentType)),
  rdf_guess_format(In, FileExtension, ContentType, Format),

  % Store the guessed RDF serialization format as metadata.
  rdf_serialization(_, Format, _, Serialization),

  % Set options: base URI, RDF serialization format, XML namespaces.
  set_stream(In, file_name(Base)),
  merge_options(
    Options1,
    [
      base_uri(Base),
      format(Format),
      graph(user),
      register_namespaces(false),
      silent(true)
    ],
    Options2
  ),

  % Add options that are specific to the RDFa serialization format.
  (   Format == rdfa
  ->  merge_options([max_errors(-1),syntax(style)], Options2, Options3)
  ;   Options3 = Options2
  ),

  % The actual loading of the RDF data.
  rdf_load(stream(In), Options3),
  % Use the default graph as given by the calling context, otherwise `user`.
  option(graph(DefaultGraph), Options3, user),

  % RDF metadata: dataset (default graph, named graphs), serialization format.
  aggregate_all(
    set(NamedGraphMetadata),
    (
      rdf_graph_property(Graph, triples(Triples)),
      Graph \== DefaultGraph,
      dict_create(NamedGraphMetadata, json, [name-Graph,triples-Triples])
    ),
    NamedGraphMetadatas
  ),
  rdf_graph_property(DefaultGraph, triples(DefaultGraphTriples)),
  rdf_statistics(graphs(Graphs)),
  rdf_statistics(literals(Literals)),
  rdf_statistics(properties(Properties)),
  rdf_statistics(resources(Resources)),
  rdf_statistics(triples(Triples)),
  Metadata2 = Metadata1.put(
    json{
      'file-extension':FileExtension,
      'RDF':json{
        dataset:json{
          'default-graph':json{triples:DefaultGraphTriples},
          'named-graphs':NamedGraphMetadatas
        },
        'number-of-graphs':Graphs,
        'number-of-unique-literals':Literals,
        'number-of-unique-properties':Properties,
        'number-of-unique-resources':Resources,
        'number-of-unique-triples':Triples,
        'serialization-format':Serialization
      }
    }
  ).





% HELPERS

%! location_suffix(+EntryMetadata, -Suffix:atom) is det.

location_suffix([filter(_)|T], Suffix):- !,
  location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix):-
  json{name:data, format:raw} :< Archive, !,
  location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix):-
  (   location_suffix(T, Suffix0)
  ->  atomic_list_concat([Archive.name, Suffix0], /, Suffix)
  ;   Suffix = Archive.name
  ).


%! metadata_content_type(+Metadata:dict, -ContentType:compound) is semidet.
% Extracts a content type term from the metadata object, if present.

metadata_content_type(Metadata, media_type(Type,Subtype,Parameters)):-
  _{type:Type, subtype:Subtype, parameters:Parameters} :< Metadata.'HTTP'.'Content-Type'.

%! metadata_to_base(+Metadata:dict, -Base:uri) is det.
%  The base URI describes the location where the data is loaded from.

metadata_to_base(Metadata, Base):-
  metadata_to_base0(Metadata, Base0),
  (   location_suffix(Metadata.archive, EntryMetadatas)
  ->  findall(
        EntryName,
        (
          member(EntryMetadata, EntryMetadatas),
          EntryName = EntryMetadata.name,
          EntryName \== data
        ),
        EntryNames
      ),
      atomic_list_concat([Base0|EntryNames], '/', Base)
  ;   Base = Base0
  ).


%! metadata_to_base0(+Metadata:dict, -Base0:atom) is det.

metadata_to_base0(Metadata, Metadata.get('URI')):- !.
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


%! rdf_extra_headers(-Headers:list(nvpair)) is det.

rdf_extra_headers([
  cert_verify_hook(ssl_verify),
  request_header('Accept'=AcceptValue)
]):-
  rdf_accept_header_value(AcceptValue).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(rdf_load_any(Metadata)) -->
  {dcg_phrase(dcg_pl_term(Metadata), Atom)},
  [Atom].
