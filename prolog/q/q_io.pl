:- module(
  q_io,
  [
  % SOURCE LAYER
    q_source_dataset/2,  % ?Dataset, -Dir
    q_source_file/3,     % ?Dataset, ?Graph, -File
    q_source_graph/1,    % -G
    q_source_graph/3,    % ?Dataset, ?Graph, -G
    q_source_ls/0,

  % SOURCE LAYER ↔ STORAGE LAYER
    q_scrape2store/1,    % +Dataset
    q_source2store/1,    % +Dataset
    q_store_rm/1,        % +G

  % STORAGE LAYER
    q_store_call/2,      % :Goal_2, +G
    q_store_file/3,      % ?Dataset, ?Graph, -File
    q_store_graph/1,     % -G
    q_store_graph/3,     % ?Dataset, ?Graph, -G
    q_store_ls/0,

  % STORAGE LAYER ↔ VIEWS LAYER
    q_store2view/2,      % +M, +G
    q_view_rm/2,         % +M, +G

  % VIEWS LAYER
    q_backend/1,         % ?M
    q_backend/2,         % ?M, ?Exts
    q_change_view/3,     % +M1, +G, +M2
    q_view_file/3,       % ?M, ?G, ?File
    q_view_graph/2,      % ?M, ?G
    q_view_ls/0,

  % VIEWS LAYER ↔ LOADED VIEWS LAYER
    q_load/2,            % +M, +G
    q_save/2,            % +M, +G
    q_unload/2,          % +M, +G

  % LOADED VIEWS LAYER
    q_loaded/2           % ?M, ?G
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
    q_ls0(+, 1),
    q_store_call(2, +).

:- multifile
    q_io:q_scrape2store_hook/2,
    q_io:q_source2store_hook/4.

:- rdf_meta
   q_change_view(+, r, +),
   q_load(+, r),
   q_loaded(?, r),
   q_save(+, r),
   q_store_call(:, r),
   q_store_graph(r),
   q_store_rm(r),
   q_store2view(+, r),
   q_unload(+, r),
   q_view_file(?, r, ?),
   q_view_graph(?, r),
   q_view_rm(+, r).





% SOURCE LAYER %

%! q_source_dataset(?Dataset, -Dir) is nondet.

q_source_dataset(Dataset, Dir) :-
  q_something_dataset(source, Dataset, Dir).



%! q_source_file(?Dataset, ?Graph, -File) is nondet.

q_source_file(Dataset, Graph, File) :-
  q_something_file(source, Dataset, Graph, File).



%! q_source_graph(-G) is nondet.
%! q_source_graph(?Dataset, ?Graph, -G) is nondet.

q_source_graph(G) :-
  q_source_graph(_, _, G).


q_source_graph(Dataset, Graph, G) :-
  q_source_file(Dataset, Graph, _),
  q_graph_name(Dataset, Graph, G).



%! q_source_ls is det.

q_source_ls :-
  q_ls0(source, q_source_graph).





% SOURCE LAYER ↔ STORAGE LAYER

%! q_scrape2store(+Dataset) is det.

q_scrape2store(Dataset) :-
  q_graph_name(Dataset, data, G),
  q_io:q_scrape2store_hook(Dataset, G).



%! q_source2store(+Dataset) is det.
%
% The resultant data file is called `<DATASET>_conv.<EXT>`.  The
% resultant VoID file, if any, is called `<NAME>_void.<EXT>`.
%
% The calls that are made are `call(<NAME>_load_data,+Sink,+G)`,
% `call(<NAME>_load_vocab,+Sink,+G)` and
% `call(<NAME>_load_void,+Sink,+G)`.

q_source2store(Dataset) :-
  forall(
    q_source_graph(Dataset, Graph, _),
    (
      q_store_file(Dataset, Graph, File2),
      exists_file(File2)
    )
  ), !,
  indent_debug(q(q_io), "Dataset ~a already exists in Quine store.", [Dataset]).
q_source2store(Dataset) :-
  forall(
    q_source_file(Dataset, Graph, File),
    (
      q_graph_name(Dataset, Graph, G),
      q_io:q_source2store_hook(Dataset, Graph, File, G)
    )
  ),
  indent_debug(q(q_io), "Dataset ~a is added to the Quine store.", [Dataset]).



%! q_store_rm(+G) is det.

q_store_rm(G) :-
  q_graph_name(Dataset, Graph, G),
  q_store_file(Dataset, Graph, File),
  delete_file(File).





% STORAGE LAYER %

%! q_store_call(:Goal_2, +G) is det.

q_store_call(Goal_2, G) :-
  q_graph_name(Dataset, Graph, G),
  q_store_file(Dataset, Graph, File),
  create_file_directory(File),
  call_to_ntriples(File, Goal_2).



%! q_store_dataset(?Dataset, -Dir) is nondet.

q_store_dataset(Dataset, Dir) :-
  q_something_dataset(store, Dataset, Dir).



%! q_store_file(?Dataset, ?Graph, -File) is nondet.
%
% Translate between graph names and files containing serializations of
% those graphs.
%
% File need not exist.

q_store_file(Dataset, Graph, Path) :-
  q_something_file(store, Dataset, Graph, Path).



%! q_store_graph(-G) is nondet.
%! q_store_graph(?Dataset, ?Graph, -G) is nondet.
%
% Enumerate graphs that are currently in the store.

q_store_graph(G) :-
  q_store_graph(_, _, G).


q_store_graph(Dataset, Graph, G) :-
  q_store_file(Dataset, Graph, File),
  exists_file(File),
  q_graph_name(Dataset, Graph, G).



%! q_store_ls is det.
%
% Overview of graphs that are currently in the store.

q_store_ls :-
  q_ls0(store, q_store_graph).





% STORAGE LAYER ↔ VIEWS LAYER %

%! q_store2view(+M, +G) is det.

% N-Triples → HDT
q_store2view(hdt, G) :-
  q_view_file(G, [nt,gz], NTriplesFile),
  exists_file(NTriplesFile), !,
  q_view_file(G, [hdt], HdtFile),
  hdt:hdt_create_from_file(HdtFile, NTriplesFile, []),
  indent_debug(q(q_io), "N-Triples → HDT").
% N-Quads → N-Triples
q_store2view(hdt, G) :-
  q_view_base(G, Base),
  atomic_list_concat(Base, [nq,gz], NQuadsFile),
  exists_file(NQuadsFile), !,
  q_view_file(rdf, G, NTriplesFile),
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



%! q_view_rm(+M, +G) is det.

q_view_rm(hdt, G) :- !,
  q_view_file(G, [hdt], HdtFile),
  (exists_file(HdtFile) -> delete_file(HdtFile) ; true),
  atomic_list_concat([HdtFile,index], ., HdtIndexFile),
  (exists_file(HdtIndexFile) -> delete_file(HdtIndexFile) ; true).
q_view_rm(rdf).





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
  with_mutex(q_io, (
    q_view_rm(M1, G),
    q_store2view(M2, G)
  )).



%! q_view_file(-File) is nondet.
%
% Enumerate existing view files.

q_view_file(File) :-
  q_something_root(view, Root),
  directory_path(Root, Dir),
  directory_path(Dir, File).



%! q_view_file(+M, +G, -File) is det.
%! q_view_file(-M, -G, +File) is det.
%
% Translate between backend+graph and File.
%
% File need not exist.

q_view_file(M, G, File) :-
  nonvar(M), nonvar(G), !,
  q_view_base(G, Base),
  q_backend(M, Exts),
  atomic_list_concat([Base|Exts], ., File).
q_view_file(M, G, File) :-
  nonvar(File), !,
  q_backend(M, Exts),
  atomic_list_concat([Base|Exts], ., File),
  directory_file_path(Dir, Graph, Base),
  directory_file_path(Root, Dataset, Dir),
  q_graph_name(Dataset, Graph, G),
  q_something_root(view, Root).
q_view_file(_, _, _) :-
  instantiation_error(q_view_file(_,_,_)).



%! q_view_graph(+M, +G) is semidet.
%! q_view_graph(-M, -G) is nondet.

q_view_graph(M, G) :-
  nonvar(M), nonvar(G), !,
  q_view_file(M, G, File),
  exists_file(File).
q_view_graph(M, G) :-
  q_view_file(File),
  q_view_file(M, G, File).



%! q_view_ls is det.

q_view_ls :-
  forall(
    q_backend(M),
    q_ls0(view, q_view_graph(M))
  ).





% VIEWS LAYER ↔ LOADED VIEW LAYER %

%! q_load(+M, +G) is det.

q_load(hdt, G) :-
  hdt_graph0(G, _, _), !.
q_load(hdt, G) :-
  q_view_file(G, [hdt], HdtFile),
  exists_file(HdtFile), !,
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph0(G, HdtFile, Hdt)),
  indent_debug(q(q_io), "HDT → open").
q_load(rdf, G) :-
  q_graph_name(Dataset, Graph, G),
  q_store_file(Dataset, Graph, NTriplesFile),
  rdf_load_file(NTriplesFile, [rdf_format(ntriples),graph(G)]).



%! q_save(+M, +G) is det.
%
% Save the contents of 〈Dataset,Graph〉 in backend M to the storage
% layer.

q_save(M, G) :-
  q_graph_name(Dataset, Graph, G),
  q_store_file(Dataset, Graph, NTriplesFile),
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

%! q_loaded(?M, ?G) is nondet.

q_loaded(hdt, G) :-
  hdt_graph0(G, _, _).
q_loaded(rdf, G) :-
  rdf_graph(G).





% HELPERS %

%! q_ls0(+Type, :Goal_1) is det.

q_ls0(Type, Goal_1) :-
  aggregate_all(
    set(X-Y),
    (
      call(Goal_1, G),
      q_graph_name(Dataset, Graph, G),
      (X = Dataset, Y = Graph ; X = Type, Y = Dataset)
    ),
    Pairs
  ),
  pairs_to_tree(Pairs, Tree),
  print_tree(Tree).



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



%! q_something_dataset(+Type, ?Dataset, -Dir) is nondet.

q_something_dataset(Type, Dataset, Dir) :-
  q_something_root(Type, Root),
  (var(Dataset) -> directory_file(Root, Dataset) ; true),
  directory_file_path(Root, Dataset, Dir).



%! q_something_file(+Type, ?Dataset, ?Graph, -File) is nondet.

q_something_file(Type, Dataset, Graph, Path) :-
  q_something_dataset(Type, Dataset, Dir),
  directory_file(Dir, File),
  atomic_list_concat([Graph|_], ., File),
  directory_file_path(Dir, File, Path).



%! q_something_root(+Type, -Root) is det.

q_something_root(Type, Root) :-
  Spec =.. [Type,.],
  absolute_file_name(Spec, Root, [access(write),file_type(directory)]).



%! q_view_base(+G, -Base) is det.

q_view_base(G, Base) :-
  q_something_root(view, Root),
  q_graph_name(Dataset, Graph, G),
  directory_file_path(Root, Dataset, Dir),
  directory_file_path(Dir, Graph, Base).
