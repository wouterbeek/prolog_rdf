:- module(
  q_io,
  [
  % GENERICS
  q_file/3,             % ?Name, ?Format, ?File
  q_file_delete/1,      % +File  
  q_file_graph/2,       % ?File, ?G
  q_file_graph/3,       % ?File, ?Format, ?G
  q_file_is_ready/1,    % +File
  q_file_is_ready/2,    % +File1, File2
  q_file_touch_ready/1, % +File
  q_graph_hash/2,       % ?G, ?Hash
  q_graph_hash/3,       % ?G, ?Name, ?Hash
  q_ls/0,

  % SOURCE
  q_source_file/1,      % -File

    % SOURCE ⬄ STORE
    q_generate/2,       % -G, :Goal_1
    q_init/0,
    q_source2store/0,
    q_store_rm/0,

  % STORE
  q_store_file/2,       % -File, ?G
  q_store_graph/1,      % -G
  q_transform/2,        % +G, :Goal_3
    
    % STORE ⬄ CACHE
    q_store2cache/0,
    q_store2cache/1,    % +M
    q_store2cache/2,    % +M, +G
    q_cache_rm/0,
    q_cache_rm/1,       % +G
    q_cache_rm/2,       % +M, +G

  % CACHE
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
  q_view_graph/1,       % ?G
  q_view_graph/2        % ?M, ?G
  ]
).

/** <module> Quine I/O

We rely on a parallel between:

  - a directory

  - a hash

  - a graph

hash --- graph
 |
 |
dir ---- file

Quads are reduced to triples because it is too difficult to support
them.

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(apply)).
:- use_module(library(base64)).
:- use_module(library(call_ext)).
:- use_module(library(conv/csv2rdf), []).  % CSV → N-Triples
:- use_module(library(conv/json2rdf), []). % JSON → N-Triples
:- use_module(library(conv/xml2rdf), []).  % XML → N-Triples
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(error)).
:- use_module(library(gis/gis), []).       % RDF → GIS
:- use_module(library(hash_ext)).
:- use_module(library(hdt/hdt_io), []).    % N-Triples → HDT
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(q/q_dataset)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf__io)). % N-Quads, RDF/XML, … → N-Triples
                                     % N-Triples → TRP
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).
:- use_module(library(tree/s_tree)).

:- meta_predicate
    q_generate(-, 1),
    q_ls0(+, 1),
    q_transform(+, 3).

:- multifile
    q_backend_hook/1,       % Only for backends that have no files in cache.
    q_cache_format_hook/2,  % E.g., ‘hdt’ file extension for HDT.
    q_cache_rm_hook/2,      % E.g., ‘hdt’ also has an index file.
    q_dataset2store_hook/1,
    q_source2store_hook/3,  % E.g., convert CSV files to N-Triple files.
    q_source_format_hook/2, % E.g., CSV files have format ‘csv’.
    q_store2cache_hook/4,   % E.g., create HDT file from N-Triples file.
    q_store2view_hook/2,    % E.g., GIS index, which has no cache format.
    q_view_graph_hook/2,    % E.g., (hdt,G)-pairs.
    q_view_rm_hook/2.

:- rdf_meta
   q_cache_graph(?, r),
   q_file_graph(?, ?, r),
   q_graph_hash(r, ?),
   q_graph_hash(r, ?, ?),
   q_source2store(r),
   q_source2store(+, r).

:- setting(
     source_dir,
     atom,
     '',
     "Directory that holds the data source files."
   ).
:- setting(
     store_dir,
     atom,
     '',
     "Directory that stores the created data files."
   ).





% GENERICS %

%! q_format(+Format) is semidet.
%! q_format(-Format) is multi.
%! q_format(+Format, -Exts) is det.
%! q_format(-Format, -Exts) is multi.

q_format(Format) :-
  q_format(Format, _).


q_format(Format, Exts) :-
  q_store_format(Format, Exts).
q_format(Format, Exts) :-
  q_cache_format(Format, Exts).



%! q_graph_hash(+G, -Hash) is det.
%! q_graph_hash(-G, +Hash) is multi.

q_graph_hash(G, Hash) :-
  q_graph_hash(G, _, Hash).


%! q_graph_hash(+G, -Name, -Hash) is det.
%! q_graph_hash(-G, +Name, +Hash) is det.
%! q_graph_hash(-G, -Name, +Hash) is multi.

q_graph_hash(G, Name, Hash) :-
  nonvar(G), !,
  rdf_global_id(Name:Hash, G).
q_graph_hash(G, Name, Hash) :-
  q_name(Name),
  rdf_global_id(Name:Hash, G).



%! q_ls is det.

q_ls :-
  setting(source_dir, Dir1),
  q_ls0(source(Dir1), q_source_file),
  setting(store_dir, Dir2),
  q_ls0(store(Dir2), q_store_graph),
  forall(
    q_backend(M),
    q_ls0(cache(M), q_cache_graph(M))
  ),
  forall(
    q_backend(M),
    q_ls0(view(M), q_view_graph(M))
  ).


q_ls0(Root, Goal_1) :-
  aggregate_all(set(Root-G), call(Goal_1, G), Pairs),
  (   pairs_to_tree(Pairs, Tree)
  ->  print_tree(Tree, [label_writer(q_io:print_term0)])
  ;   writeln("∅")
  ).

print_term0(G) -->
  {
    q_graph_hash(G, EncLocal), !,
    base64(Local, EncLocal)
  },
  atom(Local).
print_term0(Term) -->
  atom(Term).





% SOURCE %

%! q_source_format(?Format, ?Exts) is nondet.
%
% Enumerates recognized source file formats based on file extensions.
%
% Extended with hook q_source_format_hook

q_source_format(Format, Exts) :-
  q_source_format_hook(Format, Exts).



%! q_source_file(-File) is nondet.
%
% Enumerates the source files.

q_source_file(File) :-
  setting(source_dir, Dir),
  % Every non-directory file in the source directory is a source file.
  directory_path_recursive(Dir, File).





% SOURCE ⬄ STORE %

%! q_dataset2store is det.

q_dataset2store :-
  forall(
    (
      q_dataset2store_hook(D),
      q_dataset_graph(D, G)
    ),
    q_view2store(trp, G)
  ).



%! q_init is det.

q_init :-
  q_source2store,
  q_dataset2store,
  q_store2view.



%! q_source2store is det.
%
% Automatically convert source files to store files.
%
% The following options are defined:
%
%   - concept(+atom) The concept name of the main/record entity.  Used
%     in the generated IRI names.  By default this is `resource`.
%
%   - domain(+atom) The domain name used in the generated IRI names.
%     By default this is the authority component of the IRI prefix
%     registered with alias `ns`.
%
%   - name(+atom) Local name use for the IRI graph name.
%
%   - record_names(+list(atom)) For the XML source format.

q_source2store :-
  forall(
    q_source_file(File),
    q_source2store(File)
  ).


q_source2store(File1) :-
  file_extensions(File1, Exts),
  % The format is determined by the file extensions.
  (   q_source_format(Format, Exts)
  ->  true
  ;   existence_error(source_format, Exts)
  ),
  % Derive a hash based on the source file.
  setting(source_dir, Dir),
  directory_file_path(Dir, Local, File1),
  base64(Local, Hash),
  % Determine the store file.
  q_file_hash(File2, data, ntriples, Hash),
  (   q_file_ready_time(File2, Ready2),
      time_file(File1, Ready1),
      Ready2 >= Ready1
  ->  true
  ;   create_file_directory(File2),
      % Automatically convert source file to store file.
      once(q_source2store_hook(Format, File1, File2)),
      q_file_touch_ready(File2)
  ).



%! q_store_rm is det.
%! q_store_rm(+G) is det.

q_store_rm :-
  forall(
    q_store_graph(G),
    q_store_rm(G)
  ),
  % Delete empty directories.
  setting(store_dir, Dir),
  forall(
    (
      directory_recursive(Dir, Subdir),
      directory_is_empty(Subdir)
    ),
    delete_directory(Subdir)
  ).


q_store_rm(G) :-
  q_cache_rm(G),
  q_file_graph(File, G),
  delete_file_msg(File).





% STORE %

%! q_file(?Name, ?Format, -File) is nondet.
%! q_file(-Name, -Format, +File) is nondet.

q_file(Name, Format, File) :-
  ground(File), !,
  q_dir_file(Dir, Name, Format, File),
  q_dir(Dir).
q_file(Name, Format, File) :-
  q_dir(Dir),
  q_dir_file(Dir, Name, Format, File),
  exists_file(File).



%! q_file_delete(+File) is det.

q_file_delete(File) :-
  delete_file_msg(File),
  q_file_ready(File, Ready),
  delete_file_msg(Ready).



%! q_file_graph(+File, -G) is det.
%! q_file_graph(-File, +G) is multi.

q_file_graph(File, G) :-
  q_file_graph(File, _, G).


%! q_file_graph(+File, -Format, -G) is det.
%! q_file_graph(-File, -Format, +G) is multi.
%! q_file_graph(-File, +Format, +G) is det.

q_file_graph(File, Format, G) :-
  ground(File), !,
  q_file_hash(File, Name, Format, Hash),
  q_graph_hash(G, Name, Hash).
q_file_graph(File, Format, G) :-
  q_graph_hash(G, Name, Hash),
  q_file_hash(File, Name, Format, Hash).



%! q_file_is_ready(+File) is semidet.
%! q_file_is_ready(+File1, +File2) is semidet.

q_file_is_ready(File) :-
  q_file_ready(File, Ready),
  exists_file(Ready).


q_file_is_ready(File1, File2) :-
  q_file_ready_time(File2, Ready2),
  q_file_ready_time(File1, Ready1),
  Ready2 >= Ready1.



%! q_file_ready(+File, -Ready) is det.

q_file_ready(File, Ready) :-
  atomic_list_concat([File,ready], ., Ready).



%! q_file_ready_time(+File, -Time) is det.

q_file_ready_time(File, Time) :-
  q_file_ready(File, Ready),
  exists_file(Ready),
  time_file(Ready, Time).



%! q_file_touch_ready(+File) is det.

q_file_touch_ready(File) :-
  q_file_ready(File, Ready),
  touch(Ready).



%! q_generate(-G, :Goal_1) is det.

q_generate(G, Goal_1) :-
  call(Goal_1, G),
  % The transformations now have to be synced with the store, from
  % which the cache can be recreated.
  q_view2store(trp, G),
  % Sync the cached version.
  q_store2view(G).



%! q_store_format(?Format, ?Exts) is nondet.
%
% N-Triples is the only format used in the store.

q_store_format(ntriples, [nt,gz]).



%! q_store_file(-File, +G) is nondet.
%! q_store_file(-File, -G) is nondet.
%
% Enumerates the files and associated graph names that are currently
% in the store directory.

q_store_file(File, G) :-
  nonvar(G), !,
  q_file_graph(File, ntriples, G).
q_store_file(File, G) :-
  setting(store_dir, Dir),
  directory_path_recursive(Dir, File),
  q_file_graph(File, G).



%! q_store_graph(-G) is nondet

q_store_graph(G) :-
  distinct(G, q_store_file(_, G)).



%! q_transform(+G, :Goal_3) is det.
%
% Only within the ‘trp’ view can we perform arbitrary transformations.

q_transform(G, Goal_3) :-
  % Convert store to the ‘trp’ view, perform the transformation there,
  % and write the result back.
  M1 = trp,
  M2 = trp,
  q_store2view(M1, G),
  call(Goal_3, M1, M2, G),
  % The transformations now have to be synced with the store, from
  % which the cache can be recreated.
  q_view2store(M2, G),
  % Sync the cached version.
  q_store2view(G).





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
      once(q_store2cache_hook(M, File1, File2, G)),
      q_file_touch_ready(File2)
  ).



%! q_cache_rm is det.
%! q_cache_rm(+G) is det.
%! q_cache_rm(+M, +G) is det.

q_cache_rm :-
  forall(
    q_cache_graph(G),
    q_cache_rm(G)
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
      q_file_delete(File)
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
  setting(store_dir, Dir),
  atom_concat(Dir, _, File).
q_cache_file(M, File) :-
  setting(store_dir, Dir),
  directory_path_recursive(Dir, File),
  file_extensions(File, Exts),
  once(q_cache_format(M, Exts)).



%! q_cache_graph(+G) is semidet.
%! q_cache_graph(-G) is nondet.

q_cache_graph(G) :-
  distinct(G, q_cache_graph(_, G)).


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
  once(q_cache2view_hook(M, G)).



%! q_store2view is det.
%! q_store2view(+G) is det.
%! q_store2view(+M, +G) is det.

q_store2view :-
  forall(
    q_store_graph(G),
    q_store2view(G)
  ).

  
q_store2view(G) :-
  forall(
    q_backend(M),
    q_store2view(M, G)
  ).


q_store2view(M, G) :-
  q_store2view_hook(M, G), !.
q_store2view(M, G) :-
  q_store2cache(M, G),
  q_cache2view(M, G).



%! q_view2store(+M) is det.
%! q_view2store(+M, +G) is det.

q_view2store(M) :-
  forall(
    q_view_graph(M, G),
    q_view2store(M, G)
  ).


q_view2store(M, G) :-
  q_file_graph(NTriplesFile, ntriples, G),
  create_file_directory(NTriplesFile),
  rdf_write_to_sink(NTriplesFile, M, G, [mode(write),rdf_format(ntriples)]),
  q_file_touch_ready(NTriplesFile).



%! q_view_rm is det.
%! q_view_rm(+G) is det.
%! q_view_rm(+M, +G) is det.

q_view_rm :-
  forall(
    q_view_graph(G),
    q_view_rm(G)
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

%! q_view_graph(+G) is semidet.
%! q_view_graph(-G) is nondet.

q_view_graph(G) :-
  nonvar(G), !,
  once(q_view_graph(_, G)).
q_view_graph(G) :-
  distinct(G, q_view_graph(_, G)).


%! q_view_graph(+M, +G) is semidet.
%! q_view_graph(+M, -G) is nondet.
%! q_view_graph(-M, +G) is nondet.
%! q_view_graph(-M, -G) is nondet.
%
% Non-dummy views.

q_view_graph(M, G) :-
  q_backend(M),
  q_view_graph_hook(M, G, false).





% HELPERS %

%! q_dir(+Dir) is semidet.
%! q_dir(-Dir) is nondet.
%
% Directory of a data graph.

q_dir(Dir3) :-
  setting(store_dir, Dir1),
  directory_path(Dir1, Dir2),
  directory_path(Dir2, Dir3).



%! q_dir_file(+Dir, +Name, +Format, -File) is det.
%! q_dir_file(+Dir, +Name, -Format, -File) is multi.
%! q_dir_file(+Dir, -Name, +Format, -File) is multi.
%! q_dir_file(+Dir, -Name, -Format, -File) is multi.
%! q_dir_file(-Dir, -Name, -Format, +File) is det.

q_dir_file(Dir, Name, Format, File) :-
  ground(File), !,
  directory_file_path(Dir, Local, File),
  atomic_list_concat([Name|Exts], ., Local),
  q_format(Format, Exts).
q_dir_file(Dir, Name, Format, File) :-
  q_name(Name),
  (   nonvar(Format)
  ->  once(q_format(Format, Exts))
  ;   q_format(Format, Exts)
  ),
  atomic_list_concat([Name|Exts], ., Local),
  directory_file_path(Dir, Local, File).



%! q_dir_hash(+Dir, -Hash) is det.
%! q_dir_hash(-Dir, +Hash) is det.

q_dir_hash(Dir, Hash) :-
  ground(Dir), !,
  directory_subdirectories(Dir, Subdirs),
  reverse(Subdirs, [Postfix,Prefix|_]),
  atom_concat(Prefix, Postfix, Hash).
q_dir_hash(Dir4, Hash) :-
  atom_codes(Hash, [H1,H2|T]),
  maplist(atom_codes, [Dir1,Dir2], [[H1,H2],T]),
  append_directories(Dir1, Dir2, Dir3),
  setting(q_io:store_dir, Dir0),
  directory_file_path(Dir0, Dir3, Dir4).



%! q_file_hash(+File, -Hash) is det.
%! q_file_hash(-File, +Hash) is multi.

q_file_hash(File, Hash) :-
  q_file_hash(File, _, _, Hash).


%! q_file_hash(+File, -Name, -Format, -Hash) is det.
%! q_file_hash(-File, +Name, +Format, +Hash) is det.
%! q_file_hash(-File, +Name, -Format, +Hash) is multi.
%! q_file_hash(-File, -Name, +Format, +Hash) is multi.
%! q_file_hash(-File, -Name, -Format, +Hash) is multi.

q_file_hash(File, Name, Format, Hash) :-
  ground(File), !,
  q_dir_file(Dir, Name, Format, File),
  q_dir_hash(Dir, Hash).
q_file_hash(File, Name, Format, Hash) :-
  q_dir_hash(Dir, Hash),
  q_dir_file(Dir, Name, Format, File).



%! q_name(+Name) is semidet.
%! q_name(-Name) is multi.

q_name(data).
q_name(meta).
q_name(stat).
q_name(vocab).
q_name(void).
q_name(warn).
