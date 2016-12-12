:- module(
  q_io,
  [
  % SOURCE
  q_dataset2store/0,
  q_dataset2store/1,        % +Name
  q_file_name_to_format/2,  % +FileName, -Format
  q_source_dir/1,           % -Dir
  q_source_file/1,          % -File
  q_source_graph/1,         % ?G

    % SOURCE ⬄ STORE
    q_generate/2,            % ?G, :Goal_1
    q_sync/0,
    q_sync/1,                % +G
    q_init/0,
    q_init/1,                % +M
    q_init_profile/1,        % +Ms
    q_source2store/0,
    q_source2store_file/2,   % +SourceFile, -StoreFile
    q_source2store_source/3, % +Source, +Info, +Opts
    q_source2view/0,
    q_store_rm/0,
    q_store_rm/1,            % +G

  % STORE
  q_store_dir/1,        % -Dir
  q_store_file/3,       % -File, ?Base, ?G
  q_store_graph/1,      % -G
  q_store_graph/2,      % ?Base, -G
  q_transform_cbd/2,    % +G, :Goal_1
  q_transform_graph/2,  % +G, :Goal_1

    % STORE ⬄ CACHE
    q_store2cache/0,
    q_store2cache/1,    % +M
    q_store2cache/2,    % +M, +G
    q_cache_rm/0,
    q_cache_rm/1,       % +G
    q_cache_rm/2,       % +M, +G

  % CACHE
  q_backend/1,          % ?M
  q_change_cache/3,     % +M1, +G, +M2
  q_cache_file/2,       % ?M, ?File
  q_cache_graph/2,      % ?M, ?G

    % CACHE ⬄ VIEW
    q_cache2view/0,
    q_cache2view/1,     % +M
    q_cache2view/2,     % +M, +G
    q_store2view/0,
    q_store2view/1,     % +M
    q_store2view/2,     % +M, +G
    q_view2store/1,     % +M
    q_view2store/2,     % +M, +G
    q_view_rm/0,
    q_view_rm/1,        % +G
    q_view_rm/2,        % +M, +G

  % VIEW
  q_view_graph/2,       % ?M, ?G

  % DEBUG
  deb_q_io//3           % +From, +G, +To
  ]
).

/** <module> Quine I/O

# Steps

q_init/0 does three things:

  1. q_source2store/0 converts non-RDF files in /source to RDF files in /store.

  2. q_dataset2store/0 calls the currently loaded
     q_dataset2store_hook/2 clauses.  These are typically loaded from
     files in /script.  This scrapes for new data and transforms data
     already in /store.

  3. q_store2view/0 creates all supported caches and views for the RDF
     data in /store.


# Concepts

We rely on a parallel between:

  - a directory

  - a hash

  - a graph

hash --- graph
 |
 |
dir ---- file


# Simplifications

Quads are reduced to triples because it is too difficult to support
them.

---

@author Wouter Beek
@version 2016/08-2016/12
*/

:- use_module(library(conv/csv2rdf), []).  % CSV → N-Triples
:- use_module(library(conv/json2rdf), []). % JSON → N-Triples
:- use_module(library(conv/xml2rdf), []).  % XML → N-Triples
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(error)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(gis/gis), []).       % RDF → GIS
:- use_module(library(hash_ext)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(hdt/hdt_io), []).    % N-Triples → HDT
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(q/q_dataset_db)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf__io)). % N-Quads, RDF/XML, … → N-Triples
                                     % N-Triples → TRP
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).
:- use_module(library(string_ext)).
:- use_module(library(tree/s_tree)).

:- meta_predicate
    q_generate(?, 1),
    q_transform_cbd(+, 1),
    q_transform_graph(+, 1).

:- multifile
    q_backend_hook/1,       % Only for backends that have no files in cache.
    q_cache_format_hook/2,  % E.g., ‘hdt’ file extension for HDT.
    q_cache_rm_hook/2,      % E.g., ‘hdt’ also has an index file.
    q_cache2view_hook/2,
    q_dataset2store_hook/1,
    q_dataset2store_hook/2, % E.g., run custom script for ‘bgt’.
    q_source2store_hook/5,  % E.g., convert CSV files to N-Triple files.
    q_source_format_hook/2, % E.g., CSV files have format ‘csv’.
    q_store2cache_hook/4,   % E.g., create HDT file from N-Triples file.
    q_store2view_hook/2,    % E.g., GIS index, which has no cache format.
    q_view_graph_hook/3,    % E.g., (hdt,G)-pairs.
    q_view_rm_hook/2.

:- rdf_meta
   q_cache_graph(?, r),
   q_file_graph(?, ?, r),
   q_store2view(+, r),
   q_source_graph(r),
   q_view2store(+, r).

:- setting(
     source_dir,
     atom,
     '~/Data/source/',
     "Directory that holds the data source files."
   ).
:- setting(
     store_dir,
     atom,
     '~/Data/store/',
     "Directory that stores the created data files."
   ).





% SOURCE %

%! q_file_name_to_format(+File, -Format) is det.

q_file_name_to_format(File, Format) :-
  file_extensions(File, Exts),
  % The format is determined by the file extensions.
  (   q_source_format(Format, Exts)
  ->  true
  ;   q_source_skip_exts(Exts)
  ;   existence_error(source_format, Exts)
  ).


q_source_skip_exts([md]).



%! q_source_dir(-Dir) is det.

q_source_dir(Dir) :-
  setting(source_dir, Dir0),
  expand_file_name(Dir0, [Dir|_]),
  create_directory(Dir).



%! q_source_format(?Format, ?Exts) is nondet.
%
% Enumerates recognized source file formats based on file extensions.
%
% Extended with hook q_source_format_hook

% Compressed
q_source_format(Format, [Ext,gz]) :- !,
  q_source_format_hook(Format, Ext).
% Uncompressed
q_source_format(Format, [Ext]) :-
  q_source_format_hook(Format, Ext).



%! q_source_file(-File) is nondet.
%
% Enumerates the source files.

q_source_file(File) :-
  q_source_dir(Dir),
  % Every non-directory file in the source directory is a source file.
  directory_path_recursive(Dir, File),
  % Exclude certain extensions.
  file_extensions(File, Exts),
  \+ q_source_skip_exts(Exts).



%! q_source_graph(+G) is semidet.
%! q_source_graph(-G) is nondet.

q_source_graph(G) :-
  ground(G), !,
  q_dir_graph(Dir, G),
  q_dir(_, Dir).
q_source_graph(G) :-
  q_hash(Hash),
  q_graph_hash(G, Hash).



% SOURCE ⬄ STORE %

%! q_dataset2store is det.
%! q_dataset2store(+Name) is det.

q_dataset2store :-
  findall(Name, q_dataset2store_hook(Name), Names),
  concurrent_maplist(q_dataset2store, Names).


q_dataset2store(Name) :-
  q_dataset2store_hook(Name, _).



%! q_init is det.
%! q_init(+M) is det.

q_init :-
  q_source2store,
  q_store2view,
  q_dataset2store,
  q_store2view.


q_init(M) :-
  q_source2store,
  q_store2view(M),
  q_dataset2store,
  q_store2view(M).



%! q_init_profile(+Ms) is det.

q_init_profile(Ms) :-
  thread_create(q_init_profile0(Ms), _, [alias(init),detached(true)]).

q_init_profile0([M]) :-
  q_init(M).
q_init_profile0([gis,hdt]) :- !,
  q_init(hdt),
  forall(
    distinct(G, q(hdt, _, geold:geometry, _, G)),
    q_store2view(gis, G)
  ).
q_init_profile0([gis,hdt,trp]) :-
  q_init.



%! q_source2store is det.
%
% Automatically convert all source files to store files.

q_source2store :-
  forall(
    q_source_file(File),
    q_source2store_file(File, _)
  ).



%! q_source2store_file(+SourceFile, -StoreFile) is det.
%
% The following options are supported:
%
%   * base(+atom)
%
%     Base IRI.  Required.
%
%   * concept(+atom)
%
%     The concept name of the main/record entity.  Used in the
%     generated IRI names.  By default this is `resource`.
%
%   * domain(+atom)
%
%     The domain name used in the generated IRI names.  By default
%     this is the authority component of the IRI prefix registered
%     with alias `ns`.
%
%   * format(+atom)
%
%   * name(+atom)
%
%     Local name use for the IRI graph name.
%
%   * record_names(+list(atom))
%
%     For the XML source format.
%
% Result is a list of compound terms:
%
%   * triples(-nonneg)
%
%     The number of triples that were written while converting Source
%     to store.

q_source2store_file(SourceFile, StoreFile) :-
  q_file_name_to_format(SourceFile, Format),
  directory_file_path(_, Local, SourceFile),
  file_name(Local, Base),
  atomic_list_concat([DName,GName], -, Base),
  time_file(SourceFile, Ready),
  q_source2store_source(
    SourceFile,
    _{dataset: DName, format: Format, graph: GName},
    _{},
    Ready,
    StoreFile
  ).



%! q_source2store_source(+Source, +Info, +SinkOpts) is det.
%
% Info is a dictionary with keys ‘dataset’, ‘format’, ‘graph’.

q_source2store_source(Source, Info, SinkOpts) :-
  q_source2store_source(Source, Info, SinkOpts, 0, _).


q_source2store_source(Source, Info, SinkOpts1, Ready0, StoreFile) :-
  q_dataset_iri(Info.dataset, D),
  (   q_dataset_graph(D, Info.graph, G),
      q_file_graph(StoreFile, ntriples, G),
      q_file_ready_time(StoreFile, Ready),
      Ready >= Ready0
  ->  ignore(option(triples(0), SinkOpts1))
  ;   q_store_dir(Dir),
      absolute_file_name(
        uploading,
        TmpFile0,
        [access(write),relative_to(Dir)]
      ),
      thread_file(TmpFile0, TmpFile),
      q_alias_prefix(ex, Prefix),
      SourceOpts = [base_iri(Prefix)],
      merge_options(SinkOpts1, [md5(Hash)], SinkOpts2),
      once(
	q_source2store_hook(
	  Info.format,
          Source,
          TmpFile,
          SourceOpts,
          SinkOpts2
        )
      ),
      q_file_hash(StoreFile, ntriples, Hash),
      delete_file_msg(StoreFile),
      create_file_directory(StoreFile),
      rename_file(TmpFile, StoreFile),
      q_file_touch_ready(StoreFile),
      q_file_graph(StoreFile, G),
      q_dataset_add_graph(D, Info.graph, G),
      M = trp,
      md5(Info.dataset-meta, MetaHash),
      q_graph_hash(MetaG, MetaHash),
      qb_instance(M, D, void:'Dataset', MetaG),
      capitalize_string(Info.dataset, DLbl),
      qb_label(M, D, DLbl^^xsd:string, MetaG),
      qb(M, D, void:subset, G, MetaG),
      capitalize_string(Info.graph, GLbl),
      qb_label(M, G, GLbl^^xsd:string, MetaG),
      qb(M, D, void:subset, MetaG, MetaG),
      qb_label(M, MetaG, "Metadata"@en, MetaG),
      q_dataset_add_graph(D, meta, MetaG),
      q_dataset_set_default_graph(D, meta),
      q_sync(MetaG)
  ).
  


%! q_source2view is det.

q_source2view :-
  q_source2store,
  q_store2view.



%! q_store_rm is det.
%! q_store_rm(+G) is det.

q_store_rm :-
  forall(
    q_store_graph(G),
    q_store_rm(G)
  ),
  % Delete empty directories.
  q_store_dir(Dir),
  forall(
    (
      directory_recursive(Dir, Subdir),
      directory_is_empty(Subdir)
    ),
    delete_directory(Subdir)
  ),
  q_rm_datasets.


q_store_rm(G) :-
  q_cache_rm(G),
  q_file_graph(File, G),
  q_delete_file(File),
  (q_dataset_db_exists -> q_rm_dataset_graph(G) ; true).





% STORE %

%! q_generate(?G, :Goal_1) is det.
%
% Goal_1 takes care of whether or not graph G is already instantiated.

q_generate(G, Goal_1) :-
  call(Goal_1, G),
  q_sync(G).



%! q_sync is det.
%! q_sync(+G) is det.

q_sync :-
  forall(
    q_view_graph(_, G),
    q_sync(G)
  ).


q_sync(G) :-
  % The transformations now have to be synced with the store, from
  % which the cache can be recreated.
  q_view2store(trp, G),
  % Sync the cached version.
  forall(
    q_backend(M),
    q_store2view(M, G)
  ).



%! q_store_dir(-Dir) is det.

q_store_dir(Dir) :-
  setting(store_dir, Dir0),
  expand_file_name(Dir0, [Dir|_]),
  create_directory(Dir).



%! q_store_format(?Format, ?Exts) is nondet.
%
% N-Triples is the only format used in the store.

q_store_format(ntriples, [nt,gz]).



%! q_store_file(-File, -Base, +G) is nondet.
%! q_store_file(-File, -Base, -G) is nondet.
%
% Enumerates the files and associated graph names that are currently
% in the store directory.

q_store_file(File, Base, G) :-
  nonvar(G), !,
  q_file_graph(File, Base, ntriples, G).
q_store_file(File, Base, G) :-
  q_store_dir(Dir),
  Dir \== '',
  directory_path_recursive(Dir, File),
  q_file_graph(File, Base, ntriples, G).



%! q_store_graph(-G) is nondet.

q_store_graph(G) :-
  q_store_graph(_, G).


%! q_store_graph(+Base, -G) is nondet.

q_store_graph(Base, G) :-
  q_base(Base),
  distinct(G, q_store_file(_, Base, G)).



%! q_transform_cbd(+G, :Goal_1) is det.

q_transform_cbd(G, Goal_1) :-
  q_store2view(hdt, G),
  q_file_graph(File, G),
  thread_file(File, FileTmp),
  hdt_call_on_graph(G, qu_cbd_on_hdt(Goal_1, FileTmp)),
  rename_file(FileTmp, File),
  q_file_touch_ready(File).


qu_cbd_on_hdt(Goal_1, FileTmp, Hdt) :-
  call_to_ntuples(FileTmp, qu_cbd_to_stream(Goal_1, Hdt)).


qu_cbd_to_stream(Goal_1, Hdt, State, Out) :-
  forall(
    hdt_subject0(Node, Hdt),
    setup_call_cleanup(
      rdf_tmp_graph(TmpG),
      qu_cbd_entry(Node, Goal_1, TmpG, Hdt, State, Out),
      rdf_unload_graph(TmpG)
    )
  ).


qu_cbd_entry(Node, Goal_1, TmpG, Hdt, State, Out) :-
  forall(
    q_cbd_triple(hdt0, Node, Hdt, Triple),
    qb(trp, Triple, TmpG)
  ),
  call(Goal_1, TmpG),
  forall(
    q(trp, Triple, TmpG),
    gen_ntuple(Triple, State, Out)
  ).



%! q_transform_graph(+G, :Goal_1) is det.
%
% Only within the ‘trp’ view can we perform arbitrary transformations.

q_transform_graph(G, Goal_1) :-
  % Convert store to the ‘trp’ view, perform the transformation there,
  % and write the result back.
  q_store2view(trp, G),
  call(Goal_1, G),
  % The transformations now have to be synced with the store, from
  % which the cache can be recreated.
  q_view2store(trp, G),
  % Sync the cached version.
  forall(
    q_backend(M),
    q_store2view(M, G)
  ).





% STORE ⬄ CACHE %

%! q_store2cache is det.
%! q_store2cache(+M) is det.
%! q_store2cache(+M, +G) is det.
%
% Create a cache of store graph G in backend M.

q_store2cache :-
  forall(
    q_backend(M),
    q_store2cache(M)
  ).


q_store2cache(M) :-
  forall(
    q_store_graph(G),
    q_store2cache(M, G)
  ).


q_store2cache(M, G) :-
  q_file_graph(File1, ntriples, G),
  q_file_graph(File2, M, G),
  (   q_file_is_ready(File1, File2)
  ->  true
  ;   q_cache_rm(M, G),
      debug(q_io, "» store-2-cache ~a (~w)", [G,M]),
      once(q_store2cache_hook(M, File1, File2, G)),
      q_file_touch_ready(File2),
      debug(q_io, "« store-2-cache ~a (~w)", [G,M])
  ).



%! q_cache_rm is det.
%! q_cache_rm(+G) is det.
%! q_cache_rm(+M, +G) is det.

q_cache_rm :-
  forall(
    q_cache_graph(M, G),
    q_cache_rm(M, G)
  ).


q_cache_rm(G) :-
  forall(
    q_cache_graph(M, G),
    q_cache_rm(M, G)
  ).


q_cache_rm(M, G) :-
  q_view_rm(M, G),
  (   q_cache_rm_hook(M, G)
  ->  true
  ;   q_file_graph(File, M, G),
      q_delete_file(File)
  ).





% CACHE %

%! q_backend(?M) is nondet.
%
% Enumerate the currently supported backends.
%
% There are two kinds of backends:
%
%   1. Backends that have a dedicated cache format, e.g., HDT.
%
%   2. Backends that do not have a dedicated cache formats, e.g., GIS.

q_backend(M) :-
  distinct(M, q_cache_format(M, _)).
q_backend(M) :-
  q_backend_hook(M).



%! q_cache_format(?Format, ?Exts) is nondet.

q_cache_format(Format, Exts) :-
  q_cache_format_hook(Format, Exts).



%! q_change_cache(+M1, +G, +M2) is det.

q_change_cache(M1, G, M2) :-
  with_mutex(q_io, (
    q_cache_rm(M1, G),
    q_store2cache(M2, G)
  )).



%! q_cache_file(+M, +File) is nondet.
%! q_cache_file(-M, -File) is nondet.
%
% Enumerates the files and associated graph names that are currently
% in the store directory.

q_cache_file(M, File) :-
  nonvar(File), !,
  file_extensions(File, Exts),
  once(q_cache_format(M, Exts)),
  q_store_dir(Dir),
  atom_concat(Dir, _, File).
q_cache_file(M, File) :-
  q_store_dir(Dir),
  directory_path_recursive(Dir, File),
  file_extensions(File, Exts),
  once(q_cache_format(M, Exts)).



%! q_cache_graph(+M, +G) is semidet.
%! q_cache_graph(+M, -G) is nondet.
%! q_cache_graph(-M, -G) is nondet.

q_cache_graph(M, G) :-
  nonvar(G), !,
  q_file_graph(File, G),
  q_cache_file(M, File).
q_cache_graph(M, G) :-
  q_cache_file(M, File),
  q_file_graph(File, G).





% CACHE ⬄ VIEW %

%! q_cache2view is det.
%! q_cache2view(+M) is det.
%! q_cache2view(+M, +G) is det.
%
% Load graph G into backend M.

q_cache2view :-
  forall(
    q_backend(M),
    q_cache2view(M)
  ).


q_cache2view(M) :-
  forall(
    q_cache_graph(M, G),
    q_cache2view(M, G)
  ).


% View already exists: nothing to do.
q_cache2view(M, G) :-
  % Dummy views do not need to be recreated either.
  q_view_graph_hook(M, G, _), !.
% Cannot proceed: create the cache first.
q_cache2view(M, G) :-
  \+ q_cache_graph(M, G), !,
  q_store2cache(M, G),
  q_cache2view(M, G).
q_cache2view(M, G) :-
  debug(q_io, "» cache-2-view ~a (~w)", [G,M]),
  once(q_cache2view_hook(M, G)),
  debug(q_io, "« cache-2-view ~a (~w)", [G,M]).



%! q_store2view is det.
%! q_store2view(+M) is det.
%! q_store2view(+M, +G) is det.

q_store2view :-
  forall(
    q_backend(M),
    q_store2view(M)
  ).


q_store2view(gis) :- !,
  forall(
    q_store_graph(G),
    q_store2view(gis, G)
  ).
q_store2view(M) :-
  forall(
    q_store_graph(G),
    q_store2view(M, G)
  ).


q_store2view(M, G) :-
  q_store2view_hook(M, G), !.
q_store2view(M, G) :-
  q_store2cache(M, G),
  q_cache2view(M, G).



%! q_view2store is det.
%! q_view2store(+M) is det.
%! q_view2store(+M, +G) is det.

q_view2store :-
  forall(
    q_backend(M),
    q_view2store(M)
  ).


q_view2store(M) :-
  forall(
    q_view_graph(M, G),
    q_view2store(M, G)
  ).


q_view2store(M, G) :-
  q_file_graph(NTriplesFile, ntriples, G),
  create_file_directory(NTriplesFile),
  rdf_write_to_sink(
    NTriplesFile,
    M,
    G,
    [mode(write),rdf_media_type(application/'n-triples')]
  ),
  q_file_touch_ready(NTriplesFile).



%! q_view_rm is det.
%! q_view_rm(+G) is det.
%! q_view_rm(+M, +G) is det.

q_view_rm :-
  forall(
    q_view_graph(M, G),
    q_view_rm(M, G)
  ).


q_view_rm(G) :-
  forall(
    q_view_graph(M, G),
    q_view_rm(M, G)
  ).


q_view_rm(M, G) :-
  q_view_graph_hook(M, G, _), !,
  once(q_view_rm_hook(M, G)).
q_view_rm(_, _).





% VIEW %

%! q_view_graph(+M, +G) is semidet.
%! q_view_graph(+M, -G) is nondet.
%! q_view_graph(-M, +G) is nondet.
%! q_view_graph(-M, -G) is nondet.
%
% Non-dummy views.

q_view_graph(M, G) :-
  q_backend(M),
  q_view_graph_hook(M, G, false).





% DEBUG %

%! deb_q_io(+From, +G, +To)// is det.

deb_q_io(From, G, To) -->
  str(From),
  " → ",
  dcg_q_print_graph_term(G),
  " → ",
  str(To).
