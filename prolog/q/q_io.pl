:- module(
  q_io,
  [
    % CREATE
    q_create/0,
    q_create/1,       % +Name
    q_create_vocab/3, % +Refs, :Goal_2, -G
    q_create_void/4,  % +D, +Refs, :Goal_3, -G
    q_rm/0,
    
  % SOURCE
  q_source_file/1,    % -File

    % SOURCE ⬄ STORE
    q_source2store/0,
    q_source2store/2, % +File, -G
    q_source2store/3, % +File, -G, +Opts
    q_store_rm/0,

  % STORE
  q_store_file/2,     % -File, ?G
  q_store_graph/1,    % -G
  q_transform/2,      % +G, :Goal_3
  q_transform/4,      % +M1, +M2, +G, :Goal_3
    
    % STORE ⬄ CACHE
    q_store2cache/0,
    q_store2cache/1,  % +M
    q_store2cache/2,  % +M, +G
    q_cache_rm/0,

  % CACHE
  q_change_cache/3,   % +M1, +G, +M2
  q_cache_file/3,     % -M, -File, -G
  q_cache_graph/2,    % -M, -G
    
    % CACHE ⬄ VIEW
    q_cache2view/1,           % +M
    q_cache2view/2,           % +M, +G
    q_store2view/0,
    q_store2view/1,           % +M
    q_store2view/2,           % +M, +G
    q_view2store_append/1,    % +M
    q_view2store_append/2,    % +M, +G
    q_view2store_overwrite/1, % +M
    q_view2store_overwrite/2, % +M, +G
    q_view_rm/0,
    q_view_rm/1,              % +M
    q_view_rm/2,              % +M, +G

  % VIEW
  q_view_graph/2,     % ?M, ?G

  % GENERICS
    q_graph_iri/2,    % ?Refs, ?G
    q_ls/0,
    q_vocab_iri/2,    % ?Refs, -G
    q_void_iri/2      % ?Refs, -G
  ]
).

/** <module> Quine input/output

Design principles:

  1. _any_ file can be uploaded, starting with:

    a. CSV

    b. Turtle 1.1, N-Triples 1.1, N-Quads 1.1, TRiG 1.1

    c. RDFa

    d. JSON, NDJSON, JSONLD

    e. Shapefile

    f. XML, RDF/XML 1.1

  2. over _any_ protocol, starting with:

    a. file

    b. HTTP(S) download

  3. in _any_ archive format, starting with:

    a. 7zip, ar, cab, cpio, empty, GNU Tar, ISO-9660, lha, RAR, raw,
       tar, xar, zip

  4. under _any_ compression filter, starting with:

    a. bzip2, compress, gzip, grzip, lrzip, lzip, lzma, lzop, none,
       rpm, uu, xz

  5. If the file contains graphs then these are store into independent
     files.  If the file does not contain graphs then all data is
     stored in one graph.

  6. Datasets are defined as non-empty lists of graph pointers.

  7. Each graph has an IRI and a file name.  IRI and file name have a
     one-to-one mapping.

---

Source Layer

↓ q_source2store/[0,2,3]   ↑ q_store_rm/0

Storage Layer

↓ q_store2cache/[0-2]      ↑ q_cache_rm/0

Cache Layer

↓ q_cache2view/[1,2]       ↑ q_cache_rm/[0-2]

View Layer

---

Purpose of each layer

  - The source layer contains the raw sources that can be in any (also
    non-RDF) format.

  - The storage layer contains the converted data stored in a single,
    clean and standards-compliant RDF format.

  - The cache layer contains the data cached in a format for use in
    specific views.  This is not necessarily RDF, since that is not
    optimal for most applications.

  - The view layer contains data loaded for direct use in
    applications.

---

The supported source formats are extended through hooks:

  - q_source2store_hook(+M, +Source, +Sink, +Opts)

  - q_source_extensions_hook(?Format, -Exts)

---

The supported cache formats are extended through hooks:

  - q_store2cache_hook(+M, +G)

  - q_cache2view_hook(+M, +G)

  - q_cache_extensions_hook(?M, -Exts)

  - q_cache_rm_hook(+M, +G)

  - q_view_rm_hook(+M, +G)

---

Convertible datasets are loaded in through a hook:

  - q_create_hook(+Name)

---

The following flags are used:

  * q(q_io)

---

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(conv/csv2rdf), []). % CSV → N-Triples
:- use_module(library(conv/json2rdf), []). % JSON → N-Triples
:- use_module(library(conv/xml2rdf), []). % XML → N-Triples
:- use_module(library(debug_ext)).
:- use_module(library(gis/gis), []). % RDF → GIS
:- use_module(library(hdt/hdt_io), []). % N-Triples → HDT
:- use_module(library(os/file_ext)).
:- use_module(library(persistency)).
:- use_module(library(q/q_dataset)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdf__io)). % N-Quads, RDF/XML, … → N-Triples
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db), [rdf_load_db/1,rdf_save_db/2]).
:- use_module(library(tree/s_tree)).


:- initialization(db_attach('q_cache.db', [])).


:- meta_predicate
    q_create_vocab(+, 2, -),
    q_create_void(+, +, 3, -),
    q_ls(+, 1),
    q_transform(+, 3),
    q_transform(+, +, +, 3).


:- multifile
    % DATASETS
    q_create_hook/1, % E.g., create the CBS dataset.
    % SOURCE FORMATS
    q_source2store_hook/4, % E.g., convert CSV files to N-Triple files.
    q_source_extensions_hook/2, % E.g., CSV files have extensions ‘csv’.
    % CACHE FORMATS
    q_backend_hook/1, % Only for backends that have no files in cache.
    q_cache2view_hook/2, % E.g., open HDT file.
    q_cache_extensions_hook/2, % E.g., ‘hdt’ file extension for HDT.
    q_store2cache_hook/2, % E.g., create HDT file from N-Triples file.
    q_view_graph_hook/3, % E.g., (hdt,G)-pairs.
    q_view_rm_hook/2. % E.g., remove the HDT files for specific graphs.


:- persistent
   q_cache0(backend:atom, graph:atom).


:- rdf_meta
   q_cache2view(+, r),
   q_cache_file(-, -, r),
   q_cache_graph(-, r),
   q_change_cache(+, r, +),
   q_create(r),
   q_create_void(r, +, :, -),
   q_graph_iri(+, r),
   q_source2store(r),
   q_source2store(+, r),
   q_source2store(+, r, +),
   q_store2cache(+, r),
   q_store2view(+, r),
   q_store_file(?, r),
   q_store_graph(r),
   q_transform(r, :),
   q_transform(+, +, r, :),
   q_view2store_overwrite(+, r),
   q_view2store_append(+, r),
   q_view_rm(+, r),
   q_view_graph(-, r),
   q_vocab_iri(?, r),
   q_void_iri(?, r).





% CREATE %

%! q_create is det.
%! q_create(+Name) is det.

q_create :-
  forall(q_create(_)).


q_create(Name) :-
  q_create_hook(Name).



%! q_create_vocab(+Refs, :Goal_2, -G) is det.

q_create_vocab(Refs, Goal_2, G) :-
  q_vocab_iri(Refs, G),
  call(Goal_2, rdf, G),
  % Write to store&view.
  q_view2store_overwrite(rdf, G),
  q_store2view0(G).



%! q_create_void(+Refs, +D, :Goal_3, -G) is det.

q_create_void(Refs, D, Goal_3, G) :-
  q_void_iri(Refs, G),
  call(Goal_3, rdf, D, G),
  % Write to store&view.
  q_view2store_overwrite(rdf, G),
  q_store2view0(G).



%! q_rm is det.

q_rm :-
  q_store_rm,
  q_cache_rm,
  q_view_rm.





% SOURCE %

%! q_source_extensions(?Format, ?Exts) is nondet.

q_source_extensions(Format, Exts) :-
  q_source_extensions_hook(Format, Exts).



%! q_source_file(-File) is nondet.
%
% Enumerates the files currently in the source root directory.

q_source_file(File) :-
  q_root(source, Root),
  directory_path_recursive(Root, File).





% SOURCE ⬄ STORE %

%! q_source2store is det.
%! q_source2store(+File, -G) is det.
%! q_source2store(+File, -G, Opts) is det.
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
  forall(q_source_file(File), q_source2store(File, _, _{})).


q_source2store(File, G) :-
  q_source2store(File, G, _{}).


q_source2store(File, G, Opts) :-
  access_file(File, read),
  file_extensions(File, Exts),
  once(q_source_extensions(Format, Exts)),
  q_file_to_graph(source, File, G),
  q_graph_to_file(store, G, ntriples, Sink),
  q_source2store_hook(Format, File, Sink, Opts).



%! q_store_rm is det.

q_store_rm :-
  q_root_rm(store),
  q_rm_dataset,
  delete_file_msg('q_dataset.db').





% STORE %

%! q_store_extensions(?Format, ?Exts) is nondet.
%
% N-Triples is the only format used in the store.

q_store_extensions(ntriples, [nt,gz]).



%! q_store_file(-File, +G) is nondet.
%! q_store_file(-File, -G) is nondet.
%
% Enumerates the files and associated graph names that are currently
% in the store directory.

q_store_file(File, G) :-
  nonvar(G), !,
  q_graph_to_file(store, G, ntriples, File).
q_store_file(File, G) :-
  q_root(store, Root),
  directory_path_recursive(Root, File),
  q_file_to_graph(store, File, G).



%! q_store_graph(-G) is nondet

q_store_graph(G) :-
  q_store_file(_, G).



%! q_transform(+G, :Goal_3) is det.
%! q_transform(+M1, +M2, +G, :Goal_3) is det.
%
% Only within the ‘rdf’ view can we perform arbitrary transformations.

q_transform(G, Goal_3) :-
  q_transform(rdf, rdf, G, Goal_3).


q_transform(M1, M2, G, Goal_3) :-
  % Convert store to ‘rdf’ cache&view.
  q_store2view(M1, G),
  call(Goal_3, M1, M2, G),
  % The transformations now have to be synced with the store, from
  % which the cache can be recreated.
  q_view2store_overwrite(M2, G),
  % Reset the cache, otherwise store2cache will no do anything.
  q_cache_rm(M2, G),
  q_store2cache(M2, G).





% STORE ⬄ CACHE %

%! q_store2cache is det.
%! q_store2cache(+M) is det.
%! q_store2cache(+M, +G) is det.
%
% Create a cache of store graph G in backend M.

q_store2cache :-
  forall(q_backend(M), q_store2cache(M)).


q_store2cache(M) :-
  forall(q_store_graph(G), q_store2cache(M, G)).


% Nothing to do: cache already exists.
q_store2cache(M, G) :-
  q_cache0(M, G), !.
q_store2cache(M, G) :-
  q_store2cache_hook(M, G), !,
  assert_q_cache0(M, G).



%! q_cache_rm is det.
%! q_cache_rm(+M) is det.
%! q_cache_rm(+M, +G) is det.

q_cache_rm :-
  retractall_q_cache0(_, _),
  q_root_rm(cache),
  delete_file_msg('q_cache.db').


q_cache_rm(M) :-
  forall(q_cache_graph(M, G), q_cache_rm(M, G)).


q_cache_rm(M, G) :-
  retractall_q_cache0(M, G),
  q_graph_to_file(cache, G, M, File),
  delete_file_msg(File).





% CACHE %

%! q_backend(?M) is nondet.
%
% Enumerate the currently supported backends.

q_backend(M) :-
  distinct(M, q_cache_extensions(M, _)).
q_backend(M) :-
  q_backend_hook(M).



%! q_cache_extensions(?Format, ?Exts) is nondet.

q_cache_extensions(Format, Exts) :-
  q_cache_extensions_hook(Format, Exts).



%! q_change_cache(+M1, +G, +M2) is det.

q_change_cache(M1, G, M2) :-
  with_mutex(q_io, (
    q_cache_rm(M1, G),
    q_store2cache(M2, G)
  )).



%! q_cache_file(-M, -File, -G) is nondet.
%
% Enumerates the files and associated graph names that are currently
% in the store directory.

q_cache_file(M, File, G) :-
  q_root(cache, Root),
  directory_path_recursive(Root, File),
  q_file_to_graph(cache, File, G),
  file_extensions(File, Exts),
  once(q_cache_extensions(M, Exts)).



%! q_cache_graph(-M, -G) is nondet.

q_cache_graph(M, G) :-
  q_cache0(M, G).





% CACHE ⬄ VIEW %

%! q_cache2view(+M) is det.
%! q_cache2view(+M, +G) is det.
%
% Load graph G into backend M.

q_cache2view(M) :-
  forall(q_cache_graph(M, G), q_cache2view(M, G)).


% View already exists: nothing to do.
q_cache2view(M, G) :-
  % Dummy views do not need to be recreated either.
  q_view_graph(M, G, _), !.
% Cannot proceed: create the cache first.
q_cache2view(M, G) :-
  \+ q_cache_graph(M, G), !,
  q_store2cache(M, G),
  q_cache2view(M, G).
q_cache2view(M, G) :-
  q_cache2view_hook(M, G), !.



%! q_store2view is det.
%! q_store2view(+M) is det.
%! q_store2view(+M, +G) is det.

q_store2view :-
  forall(q_backend(M), q_store2view(M)).

  
q_store2view(M) :-
  forall(q_store_graph(G), q_store2view(M, G)).


q_store2view(M, G) :-
  q_store2cache(M, G),
  q_cache2view(M, G).



q_store2view0(G) :-
  forall(q_backend(M), q_store2view(M, G)).



%! q_view2store_append(+M) is det.
%! q_view2store_append(+M, +G) is det.
%
% Save the contents of 〈Dataset,Graph〉 in backend M to the storage
% layer.

q_view2store_append(M) :-
  q_view2store0(M, []).


q_view2store_append(M, G) :-
  q_view2store0(M, G, []).



%! q_view2store_overwrite(+M) is det.
%! q_view2store_overwrite(+M, +G) is det.

q_view2store_overwrite(M) :-
  q_view2store0(M, [mode(write)]).


q_view2store_overwrite(M, G) :-
  q_view2store0(M, G, [mode(write)]).


q_view2store0(M, Opts) :-
  forall(q_view_graph(M, G), q_view2store0(M, G, Opts)).


q_view2store0(M, G, Opts1) :-
  q_graph_to_file(store, G, ntriples, NTriplesFile),
  merge_options(Opts1, [rdf_format(ntriples)], Opts2),
  create_file_directory(NTriplesFile),
  rdf_write_to_sink(NTriplesFile, M, G, Opts2).



%! q_view_rm is det.
%! q_view_rm(+M) is det.
%! q_view_rm(+M, +G) is det.

q_view_rm :-
  forall(q_backend(M), q_view_rm(M)).


q_view_rm(M) :-
  % Also dummy views are removed.
  forall(q_view_graph(M, G), q_view_rm(M, G)).


q_view_rm(M, G) :-
  q_view_rm_hook(M, G), !.





% VIEW %

%! q_view_graph(+M, +G) is semidet.
%! q_view_graph(+M, -G) is nondet.
%! q_view_graph(-M, +G) is nondet.
%! q_view_graph(-M, -G) is nondet.
%
% Non-dummy views.

q_view_graph(M, G) :-
  q_view_graph(M, G, false).


q_view_graph(M, G, Dummy) :-
  q_view_graph_hook(M, G, Dummy).





% GENERICS %

%! q_graph_iri(+Refs, -G) is det.

q_graph_iri(Refs, G) :-
  q_alias_domain(ns, Domain),
  q_abox_iri(Domain, graph, Refs, G).



%! q_ls is det.

q_ls :-
  q_ls(source, q_source_file),
  q_ls(store, q_store_graph),
  forall(q_backend(M), q_ls(cache(M), q_cache_graph(M))),
  forall(q_backend(M), q_ls(view(M), q_view_graph(M))).



%! q_vocab_iri(?Refs, -G) is det.

q_vocab_iri(Refs, G) :-
  q_graph_iri([vocab|Refs], G).



%! q_void_iri(+Refs, -G) is det.

q_void_iri(Refs, G) :-
  q_graph_iri([void|Refs], G).





% HELPERS %

%! q_extensions(+Type, +Format, -Exts) is det.

q_extensions(cache, Format, Exts) :-
  q_cache_extensions(Format, Exts).
q_extensions(source, Format, Exts) :- !,
  q_source_extensions(Format, Exts).
q_extensions(store, Format, Exts) :- !,
  q_store_extensions(Format, Exts).



%! q_file_to_graph(+Type, +File, -G) is det.

q_file_to_graph(Type, File, G) :-
  q_root(Type, Root),
  atom_concat(Root, File0, File),
  file_name(File0, Base),
  atomic_list_concat([''|Refs], /, Base),
  q_graph_iri(Refs, G).



%! q_graph_to_base(+Type, +G, -Base) is det.

q_graph_to_base(Type, G, Base) :-
  q_root(Type, Root),
  uri_components(G, Comps),
  uri_data(path, Comps, Path),
  atomic_list_concat(['',id,graph|Refs], /, Path),
  atomic_list_concat(Refs, /, Local),
  directory_file_path(Root, Local, Base).



%! q_graph_to_file(+Type, +G, +Format, -File) is det.

q_graph_to_file(Type, G, Format, File) :-
  q_graph_to_base(Type, G, Base),
  once(q_extensions(Type, Format, Exts)),
  atomic_list_concat([Base|Exts], ., File).



%! q_ls(+Root, :Goal_1) is det.

q_ls(Root, Goal_1) :-
  aggregate_all(set(Root-G), call(Goal_1, G), Pairs),
  (   pairs_to_tree(Pairs, Tree)
  ->  print_tree(Tree, [label_writer(q_print:dcg_q_print_something)])
  ;   writeln("∅")
  ).



%! q_root(+Type, -Root) is det.

q_root(Type, Root) :-
  Type =.. [Type0|_],
  Spec =.. [Type0,.],
  absolute_file_name(Spec, Root, [access(write),file_type(directory)]).



%! q_root_rm(+Type) is det.

q_root_rm(Type) :-
  q_root(Type, Root),
  directory_file_path(Root, *, Wildcard),
  forall(
    wildcard_file(Wildcard, File),
    (   exists_directory(File)
    ->  delete_directory_msg(File)
    ;   exists_file(File)
    ->  delete_file_msg(File)
    )
  ).
