:- module(
  q_io,
  [
  % SOURCE LAYER
    q_source_dataset/2,   % ?D, -Dir
    q_source_graph/1,     % ?G
    q_source_ls/0,

  % SOURCE LAYER ↔ STORAGE LAYER
    q_scrape2store/1,     % +D
    q_source2store/1,     % +D
    q_store_rm_dataset/1, % +D
    q_store_rm_graph/1,   % +G

  % STORAGE LAYER
    q_store_call/2,       % :Goal_2, +G
    q_store_dataset/2,    % ?D, -Dir
    q_store_graph/1,      % ?G
    q_store_ls/0,

  % STORAGE LAYER ↔ VIEWS LAYER
    q_store2view/2,       % +M, +G
    q_view_rm_dataset/2,  % +M, +D
    q_view_rm_graph/2,    % +M, +G

  % VIEWS LAYER
    q_backend/1,          % ?M
    q_backend/2,          % ?M, ?Exts
    q_change_view/3,      % +M1, +G, +M2
    q_view_graph/2,       % +M, ?G
    q_view_ls/0,

  % VIEWS LAYER ↔ LOADED VIEWS LAYER
    q_load/2,             % +M, +G
    q_save/2,             % +M, +G
    q_unload/2,           % +M, +G

  % LOADED VIEWS LAYER
    q_loaded_graph/2,     % ?M, ?G
    q_loaded_ls/0
  ]
).

/** <module> Quine file system


**Source layer** contains the raw sources that can be in any (also
**non-RDF) format.

Files are stored as `/source/<DATATSET>.tar.gz` and contain entries of
the form `<GRAPH>.<EXT>`.

   ↓ q_source2store/[1,2]   ↑ q_store_rm/[1,2]


**Storage layer** contains the converted data stored in a single,
clean and standards-compliant RDF format.

Files are stored as `/data/<DATASET>/<ENTRY>.nt.gz`.

   ↓ q_store2view/[2,3]   ↑ q_view_rm/[2,3]


**Views layer** contains the data stored in a format for use in
**specific applications.  This is not necessarily RDF, since that is
**not optimal for most applications.

Graph name `http://<CUSTOMER>.triply.cc/<DATASET>/<ENTRY>`

---

The following flags are used:

  * q(q_io)

---

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(debug_ext)).
:- use_module(library(error)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/directory_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(tree/s_tree)).

%! hdt_graph0(?G, ?HdtFile, ?Hdt) is nondet.

:- qb_alias(triply, 'http://triply.cc/').

:- dynamic
    hdt_graph0/3.

:- meta_predicate
    q_store_call(2, +).

:- multifile
    q_io:q_scrape2store_hook/2,
    q_io:q_source2store_hook/4.

:- rdf_meta
   q_change_view(+, r, +),
   q_load(+, r),
   q_loaded_graph(?, r),
   q_save(+, r),
   q_scrape2store(r),
   q_source2store(r),
   q_source_dataset(r, -),
   q_source_graph(r),
   q_store_call(:, r),
   q_store_dataset(r, -),
   q_store_graph(r),
   q_store_graph(?, ?, r, -),
   q_store_rm_dataset(r),
   q_store_rm_graph(r),
   q_store2view(+, r),
   q_unload(+, r),
   q_view_graph(+, r),
   q_view_graph(+, ?, ?, r, -),
   q_view_rm_dataset(+, r),
   q_view_rm_graph(+, r).





% SOURCE LAYER %

%! q_source_dataset(?D, -Dir) is nondet.

q_source_dataset(D, Dir) :-
  q_dataset(source, D, Dir).



%! q_source_graph(?G) is nondet.

q_source_graph(G) :-
  q_graph(source, Dataset, Graph, G, File).



%! q_source_ls is det.

q_source_ls :-
  q_ls(source).





% SOURCE LAYER ↔ STORAGE LAYER

%! q_scrape2store(+D) is det.

q_scrape2store(D) :-
  q_dataset_name(Dataset, D),
  q_graph_name(Dataset, data, G),
  q_io:q_scrape2store_hook(G).



%! q_source2store(+D) is det.
%
% The resultant data file is called `<DATASET>_conv.<EXT>`.  The
% resultant VoID file, if any, is called `<NAME>_void.<EXT>`.
%
% The calls that are made are `call(<NAME>_load_data,+Sink,+G)`,
% `call(<NAME>_load_vocab,+Sink,+G)` and
% `call(<NAME>_load_void,+Sink,+G)`.

q_source2store(D) :-
  q_dataset_name(Dataset, D),
  forall(
    q_graph(source, Dataset, Graph, _, _),
    (
      q_graph(store, Dataset, Graph, _, File2),
      exists_file(File2)
    )
  ), !,
  indent_debug(q(q_io), "Dataset ~a already exists in Quine store.", [Dataset]).
q_source2store(D) :-
  q_dataset_name(Dataset, D),
  forall(
    q_graph(source, Dataset, Graph, _, File),
    (
      q_graph_name(Dataset, Graph, G),
      q_io:q_source2store_hook(File, G)
    )
  ),
  indent_debug(q(q_io), "Dataset ~a is added to the Quine store.", [Dataset]).



%! q_store_rm_dataset(+Dataset) is det.

q_store_rm_dataset(D) :-
  q_rm_dataset(store, D).



%! q_store_rm_graph(+G) is det.

q_store_rm_graph(G) :-
  q_rm_graph(store, G).





% STORAGE LAYER %

%! q_store_call(:Goal_2, +G) is det.

q_store_call(Goal_2, G) :-
  q_graph_file(store, G, File),
  call_to_ntriples(File, Goal_2).



%! q_store_dataset(?Dataset, -Dir) is nondet.

q_store_dataset(D, Dir) :-
  q_dataset(store, D, Dir).



%! q_store_graph(?G) is nondet.
%! q_store_graph(?Dataset, ?Graph, ?G, -File) is nondet.
%
% Translate between graph names and files containing serializations of
% those graphs.
%
% File need not exist.

q_store_graph(G) :-
  q_store_graph(_, _, G, _).


q_store_graph(Dataset, Graph, G, File) :-
  q_graph(store, Dataset, Graph, G, File).



%! q_store_ls is det.
%
% Overview of graphs that are currently in the store.

q_store_ls :-
  q_ls(store).





% STORAGE LAYER ↔ VIEWS LAYER %

%! q_store2view(+M, +G) is det.

% N-Triples → HDT
q_store2view(hdt, G) :-
  q_view_graph(rdf, Dataset, Graph, G, NTriplesFile),
  exists_file(NTriplesFile), !,
  q_view_graph(hdt, Dataset, Graph, G, HdtFile),
  hdt:hdt_create_from_file(HdtFile, NTriplesFile, []),
  indent_debug(q(q_io), "N-Triples → HDT").
% N-Quads → N-Triples
q_store2view(hdt, G) :-
  q_view_graph(rdf, _, _, G, NTriplesFile),
  file_change_extension(NTriplesFile, 'nq.gz', NQuadsFile),
  exists_file(NQuadsFile), !,
  setup_call_cleanup(
    rdf_change_format(
      NQuadsFile,
      NTriplesFile,
      [from_format(nquads),to_format(ntriples)]
    ),
    (
      indent_debug(q(q_io), "N-Quads → N-Triples"),
      q_store2view(hdt, G)
    ),
    delete_file(NTriplesFile)
  ).
q_store2view(rdf, _).



%! q_view_rm_graph(+M, +D) is det.
%
% @tbd Use q_rm_dataset/2 once the index file is removed from the HDT
% implementation.

q_view_rm_dataset(M, D) :-
  q_dataset_name(Dataset, D),
  forall(
    q_view_graph(M, Dataset, Graph, G, _),
    q_view_rm_graph(M, G)
  ).



%! q_view_rm_graph(+M, +G) is det.
%
% @tbd Use q_rm_graph/2 for the HDT backend once the index file is
% removed from the implementation.

q_view_rm_graph(hdt, G) :- !,
  q_view_graph(hdt, _, _, G, HdtFile),
  (exists_file(HdtFile) -> delete_file(HdtFile) ; true),
  atomic_list_concat([HdtFile,index], ., HdtIndexFile),
  (exists_file(HdtIndexFile) -> delete_file(HdtIndexFile) ; true).
q_view_rm_graph(rdf, _).





% VIEWS LAYER %

%! q_backend(?M) is nondet.
%! q_backend(?M, ?Exts) is nondet.
%
% Enumerate the currently supported backends.

q_backend(M) :-
  distinct(M, q_backend(M, _)).


q_backend(rdf, [nt,gz]).
q_backend(hdt, [hdt]).



%! q_change_view(+M1, +G, +M2) is det.
%
% Load graph G that is already loaded into backend M1 into backend M2
% as well.

q_change_view(M1, G, M2) :-
  q_graph_name(Dataset, Graph, G),
  with_mutex(q_io, (
    q_graph_rm(view(M1), Dataset, Graph),
    q_store2view(M2, G)
  )).



%! q_view_graph(+M, ?Dataset, ?Graph, ?G, -File) is semidet.

q_view_graph(M, Dataset, Graph, G, File) :-
  q_graph(type(M), Dataset, Graph, G, File).



%! q_view_ls is det.

q_view_ls :-
  forall(
    q_backend(M),
    q_ls(view(M))
  ).



%! q_view_rm_dataset(+M, +D) is det.

q_view_rm_dataset(M, D) :-
  q_rm_dataset(view(M), D).





% VIEWS LAYER ↔ LOADED VIEW LAYER %

%! q_load(+M, +G) is det.

q_load(hdt, G) :-
  hdt_graph0(G, _, _), !.
q_load(hdt, G) :-
  q_view_graph(hdt, _, _, G, HdtFile),
  exists_file(HdtFile), !,
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph0(G, HdtFile, Hdt)),
  indent_debug(q(q_io), "HDT → open").
q_load(rdf, G) :-
  q_store_graph(_, _, G, NTriplesFile),
  rdf_load_file(NTriplesFile, [rdf_format(ntriples),graph(G)]).



%! q_save(+M, +G) is det.
%
% Save the contents of 〈Dataset,Graph〉 in backend M to the storage
% layer.

q_save(M, G) :-
  q_store_graph(_, _, G, NTriplesFile),
  create_file_directory(NTriplesFile),
  rdf_write_to_sink(NTriplesFile, M, G, [rdf_format(ntriples)]).



%! q_unload(+M, +G) is det.

q_unload(hdt, G) :- !,
  with_mutex(q_io, (
    hdt_graph0(G, _, Hdt),
    hdt:hdt_close(Hdt),
    retract(hdt_graph0(G,_,Hdt))
  )).
q_unload(rdf, G) :-
  rdf_unload_graph(G).





% LOADED VIEW LAYER %

%! q_loaded_graph(?M, ?G) is nondet.

q_loaded_graph(hdt, G) :-
  hdt_graph0(G, _, _).
q_loaded_graph(rdf, G) :-
  rdf_graph(G).



%! q_loaded_ls is det.

q_loaded_ls :-
  forall(
    q_backend(M),
    q_view_ls(M)
  ).





% HELPERS %

%! q_dataset(+Type, ?D, -Dir) is nondet.

q_dataset(Type, D, Dir) :-
  q_root(Type, Root),
  (   var(D)
  ->  directory_file(Root, Dataset),
      rdf_global_id(triply:Dataset, D)
  ;   rdf_global_id(triply:Dataset, D)
  ),
  directory_file_path(Root, Dataset, Dir).



%! q_dataset_file(+Type, +D, -Dir) is det.

q_dataset_file(Type, D, Dir) :-
  q_root(Type, Root),
  q_dataset_name(Dataset, D),
  directory_file_path(Root, Dataset, Dir),
  create_directory(Dir).



%! q_dataset_name(?Dataset, ?D) is nondet.

q_dataset_name(Dataset, D) :-
  rdf_global_id(triply:Dataset, D).



%! q_file_extensions(+Type, -Exts) is det.

q_file_extensions(store, [nt,gz]) :- !.
q_file_extensions(view(M), Exts) :-
  q_backend(M, Exts).



%! q_graph(+Type, ?Dataset, ?Graph, ?G, -File) is nondet.
%
% From 8 to 2 instantiation patterns.

q_graph(Type, Dataset, Graph, G, Path) :-
  nonvar(Dataset), nonvar(Graph), nonvar(G), !,
  q_graph0(Type, Dataset, Graph, G, Path).
q_graph(Type, Dataset, Graph, G, Path) :-
  (nonvar(G) ; nonvar(Dataset), nonvar(Graph)), !,
  q_graph_name(Dataset, Graph, G),
  q_graph0(Type, Dataset, Graph, G, Path).
q_graph(Type, Dataset, Graph, G, Path) :-
  q_graph0(Type, Dataset, Graph, G, Path).


%! q_graph0(+Type, +Dataset, +Graph, +G, -Path) is det.
%! q_graph0(+Type, -Dataset, -Graph, -G, -Path) is nondet.

q_graph0(Type, Dataset, Graph, G, Path) :-
  q_dataset(Type, Dataset, Dir),
  (   exists_directory(Dir)
  ->  directory_file(Dir, File),
      atomic_list_concat([Graph|_], ., File)
  ;   Type == store,
      nonvar(Graph)
  ->  atomic_list_concat([Graph,nt,gz], ., File)
  ;   Type =.. [view|M],
      q_backend(M, Exts)
  ->  atomic_list_concat([Graph|Exts], ., File)
  ),
  q_graph_name(Dataset, Graph, G),
  directory_file_path(Dir, File, Path).


  
%! q_graph_file(+Type, +G, -File) is det.

q_graph_file(Type, G, File) :-
  q_graph_name(Dataset, Graph, G),
  q_dataset_file(Type, Dataset, Dir),
  directory_file_path(Dir, Graph, Base),
  q_file_extensions(Type, Exts),
  atomic_list_concat([Base|Exts], ., File).



%! q_graph_name(+Dataset, +Graph, -G) is det.
%! q_graph_name(-Dataset, -Graph, +G) is det.

q_graph_name(Dataset, Graph, G) :-
  nonvar(G), !,
  rdf_global_id(triply:Local, G),
  atomic_list_concat([Dataset,Graph], '_', Local).
q_graph_name(Dataset, Graph0, G) :-
  nonvar(Dataset),
  nonvar(Graph0), !,
  % Sometimes we need to remove the source extensions of the entry.
  atomic_list_concat([Graph|_], ., Graph0),
  atomic_list_concat([Dataset,Graph], '_', Local),
  rdf_global_id(triply:Local, G).
q_graph_name(_, _, _) :-
  instantiation_error(q_graph_name(_,_,_)).



%! q_ls(+Type) is det.

q_ls(Type) :-
  aggregate_all(
    set(X-Y),
    (
      q_graph(Type, Dataset, Graph, _, _),
      (X = Dataset, Y = Graph ; X = Type, Y = Dataset)
    ),
    Pairs
  ),
  (pairs_to_tree(Pairs, Tree) -> print_tree(Tree) ; writeln("∅")).



%! q_rm_dataset(+Type, +D) is det.

q_rm_dataset(Type, D) :-
  q_dataset_name(Dataset, D),
  forall(
    q_graph(Type, Dataset, Graph, _, _),
    q_rm_graph(Type, Dataset, Graph)
  ).



%! q_rm_graph(+Type, +G) is det.

q_rm_graph(Type, G) :-
  q_graph(Type, _, _, G, File),
  (exists_file(File) -> delete_file(File) ; true).



%! q_root(+Type, -Root) is det.

q_root(Type, Root) :-
  Type =.. [Type0|_],
  Spec =.. [Type0,.],
  absolute_file_name(Spec, Root, [access(write),file_type(directory)]).
