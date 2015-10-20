:- module(
  rdf_load_any,
  [
    metadata_to_base/2, % +Metadata:dict
                        % -Base:uri
    rdf_load_any/2 % +Spec:compound
                   % +Options:list(nvpair)
  ]
).

:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_zlib_plugin)).

:- predicate_options(metadata_content_type/3, 3, [
  media_type(+dict)
]).
:- predicate_options(rdf_load_any/2, 2, [
  meta_data(-dict),
  pass_to(open_any/3, 3)
]).
:- predicate_options(rdf_load_any/3, 3, [
  pass_to(rdf_http_plugin:rdf_extra_headers/2, 2),
  pass_to(rdf_load_from_stream_nondet/3, 3)
]).
:- predicate_options(rdf_load_from_stream_det/4, 4, [
  filename(+atom),
  pass_to(metadata_content_type/3, 3),
  pass_to(rdf_load/2, 2)
]).
:- predicate_options(rdf_load_from_stream_nondet/3, 3, [
  silent(+boolean),
  pass_to(rdf_load_from_stream_det/4, 4)
]).

%! rdf_load_any(+Spec:compound, +Option:list(nvpair)) is det.
% Load RDF from a stream, a URL, a file, a list of files, or a file directory.
%
% `Spec` can be one of the following:
%   * file(+atom)
%   * file_pattern(+atom)
%   * file_spec(+compound)
%   * graph(+atom)
%   * prefix(+atom)
%   * stream(+stream)
%   * uri(+atom)
%   * uri_components(+compound)
%
% The following options are supported:
%   * format(+Format:oneof([ntriples,turtle,xml]))
%     The RDF serialization that has to be used for parsing.
%     Default: @tbd
%   * graph(+Graph:atom)
%     The name of the RDF graph in which the parsed data is loaded.
%     Default: `user`.
%   * keep_file(+Keep:boolean)
%     If the given input is a URL, the remote document is first
%     downloaded, and then kept available, locally.
%     Default: `false`.
%   * meta_data(-Metadata:dict)
%   * reduced_locations(+Use:boolean)
%     Whether reduced locations are used of not.
%     See [rdf_prefixes].
%     Default: `false`.
%   * silent(+boolean)
%     Whether informational messages about loaded content are shown.
%     Default: `true`.
%   * void(+Load:boolean)
%     Whether the loaded data should be recursively closed under
%     VoID descriptions that appear in that data.
%     Default: `false`.

rdf_load_any(In, Options):-
  rdf_load_any(In, Metadata, Options),
  [Entry|_] = Metadata.entries,
  ignore(option(meta_data(Metadata), Options)),
  ignore(option(metadata(Metadata), Options)),
  
  % Process the format option.
  (   option(format(Format), Options),
      var(Format)
  ->  rdf_serialization_resource(Entry.'RDF'.'serialization-format', Format)
  ;   true
  ),

  % Process the graph option.
  (   option(graph(Graph), Options)
  ->  Graph = Entry.'RDF'.dataset.'default-graph'.graph
  ;   true
  ).

% 1. Load the URI denoted by a registered RDF prefix.
rdf_load_any(prefix(Prefix), M, Options1):-
  atom(Prefix),
  rdf_current_prefix(Prefix, Uri), !,
  merge_options(Options1, [graph(Prefix)], Options2),
  rdf_load_any(uri(Uri), M, Options2).

% 2. Reuse the versatile open_any/4.
rdf_load_any(In, json{entries:EntryMetadatas}, Options1):-
  rdf_http_plugin:rdf_extra_headers(ExtraHeaders, Options1),
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
  StreamMetadata = RdfMetadata.put(stream, CloseMetadata).



%! rdf_load_from_stream_det(
%!   +In:stream,
%!   +OldMetadata:dict,
%!   -NewMetadata:dict,
%!   +Options:list(nvpair)
%! ) is det.

rdf_load_from_stream_det(In, Metadata1, Metadata2, Options1):-
  metadata_to_base(Metadata1, Base),

  % Return the file name extension as metadata.
  (   option(filename(FileName), Options1)
  ->  true
  ;   FileName = Base
  ),
  ignore(file_name_extension(_, FileExtension, FileName)),

  % Determine the RDF serialization format.
  (   option(format(Format), Options1),
      ground(Format)
  ->  true
  ;   % Guess the RDF serialization format based on
      % the HTTP Content-Type header value and the file name extension.
      ignore(metadata_content_type(Metadata1, Options1, ContentType)),
      rdf_guess_format(In, FileExtension, ContentType, Format)
  ),

  % Store the RDF serialization format as metadata.
  rdf_serialization_resource(Serialization, Format),

  % Set options: base URI, RDF serialization format, XML namespaces.
  set_stream(In, file_name(Base)),
  merge_options(
    [
      base_uri(Base),
      format(Format),
      register_namespaces(false),
      silent(true)
    ],
    Options1,
    Options2
  ),

  % Add options that are specific to the RDFa serialization format.
  (   Format == rdfa
  ->  merge_options([max_errors(-1),syntax(style)], Options2, Options3)
  ;   Options3 = Options2
  ),

  % The actual loading of the RDF data.
  rdf_load(stream(In), Options3),
  % Use the default graph as given by the calling context,
  % otherwise use the base URI.
  option(graph(DefaultGraph), Options3, Base),

  % RDF metadata: dataset (default graph, named graphs), serialization format.
  aggregate_all(
    set(named_graph{graph:Graph,name:Graph,triples:Triples}),
    (
      rdf_graph_property(Graph, triples(Triples)),
      Graph \== DefaultGraph
    ),
    NamedGraphDicts
  ),
  rdf_graph_property(DefaultGraph, triples(DefaultGraphTriples)),
  rdf_statistics(graphs(Graphs)),
  rdf_statistics(literals(Literals)),
  rdf_statistics(properties(Predicates)),
  rdf_statistics(resources(Resources)),
  rdf_statistics(triples(Triples)),
  Metadata2 = Metadata1.put(
    _{
      'file-extension':FileExtension,
      'RDF':'RDF'{
        dataset:dataset{
          'default-graph':'default-graph'{
            graph:DefaultGraph,
            triples:DefaultGraphTriples
          },
          'named-graphs':NamedGraphDicts
        },
        'number-of-graphs':Graphs,
        'number-of-unique-literals':Literals,
        'number-of-unique-predicates':Predicates,
        'number-of-unique-resources':Resources,
        'number-of-unique-triples':Triples,
        'serialization-format':Serialization
      }
    }
  ).





% HELPERS %

%! location_suffix(+EntryMetadata, -Suffix:atom) is det.

location_suffix([filter(_)|T], Suffix):- !,
  location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix):-
  metadata{name:data, format:raw} :< Archive, !,
  location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix):-
  (   location_suffix(T, Suffix0)
  ->  atomic_list_concat([Archive.name, Suffix0], /, Suffix)
  ;   Suffix = Archive.name
  ).


%! metadata_content_type(
%!   +Metadata:dict,
%!   +Options:list(nvpair),
%!   -ContentType:dict
%! ) is semidet.
% Extracts a content type term from the metadata object, if present.

metadata_content_type(_, Options, MediaType):-
  option(media_type(MediaType), Options), !.
metadata_content_type(
  Metadata,
  _,
  Metadata.get('HTTP').headers.get('Content-Type')
).

%! metadata_to_base(+Metadata:dict, -Base:uri) is det.
% The base URI describes the location where the data is loaded from.

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
      atomic_list_concat([Base0|EntryNames], /, Base)
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
