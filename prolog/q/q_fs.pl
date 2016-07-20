:- module(
  q_fs,
  [
  % SOURCE LAYER
    q_source/1,       % ?Dataset
    q_source/2,       % ?Dataset, ?Graph
    q_source_file/2,  % ?Dataset, -File
    q_source_ls/0,

  % SOURCE LAYER ↔ STORAGE LAYER
    q_source2store/1, % +Dataset
    q_source2store/2, % +Dataset, +Graph
    q_store_rm/1,     % +Dataset
    q_store_rm/2,     % +Dataset, +Graph

  % STORAGE LAYER
    q_store/1,        % ?Dataset
    q_store/2,        % ?Dataset, ?Graph
    q_store_call/2,   % :Goal_2, +G
    q_store_call/3,   % :Goal_2, +Dataset, +Graph
    q_store_dir/2,    % ?Dataset, -Dir
    q_store_file/3,   % ?Dataset, ?Graph, -File
    q_store_ls/0,
    
  % STORAGE LAYER ↔ VIEWS LAYER
    q_2view/2,        % +M, +Dataset
    q_2view/3,        % +M, +Dataset, +Graph
    q_store2view/2,   % +M, +Dataset
    q_store2view/3,   % +M, +Dataset, +Graph
    q_view_rm/2,      % +M, +Dataset
    q_view_rm/3,      % +M, +Dataset, +Graph

  % VIEWS LAYER
    q_backend/1,      % ?M
    q_change_view/3,  % +M1, +Dataset, +M2
    q_change_view/4,  % +M1, +Dataset, +Graph, +M2
    q_view/2,         % ?M, ?G
    q_view/3,         % ?M, ?Dataset, ?Graph
    q_view_file/3,    % +G, +Exts, -File
    q_view_file/4,    % +Dataset, +Graph, +Exts, -File
    q_view_ls/0,

  % VIEWS LAYER ↔ LOADED VIEWS LAYER
    q_load/2,         % +M, +G
    q_load/3,         % +M, +Dataset, +Graph
    q_save/2,         % +M, +Dataset
    q_save/3,         % +M, +Dataset, +Graph
    q_unload/2,       % +M, +G
    q_unload/3,       % +M, +Dataset, +Graph

  % LOADED VIEWS LAYER
    q_loaded/2,        % ?M, ?G
    q_loaded/3         % ?M, ?Dataset, ?Graph
  ]
).

/** <module> Quine file system


**Source layer**

`/source/<DATATSET>.tar.gz` containing `<GRAPH>.<EXT>`, …


   ↓ q_source2store/[1,2]   ↑ q_store_rm/[1,2]


**Storage layer**

`/data/<DATASET>/<GRAPH>.nt.gz`


   ↓ q_store2view/[2,3]   ↑ q_view_rm/[2,3]


**Views layer**

HDT or RDF, graph name `http://<CUSTOMER>.triply.cc/<DATASET>/<GRAPH>`


---

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(debug)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(tree/s_tree)).
:- use_module(library(yall)).

%! hdt_graph0(?G, ?HdtFile, ?Hdt) is nondet.

:- dynamic
    hdt_graph0/3.

:- meta_predicate
    q_ls0(2),
    q_store_call(2, +),
    q_store_call(2, +, +).

:- multifile
  q_fs:q_source2store_hook/4.

:- rdf_meta
    q_store_call(:, r),
    q_store_call(:, +, +).




% SOURCE LAYER %

%! q_source(?Dataset) is nondet.
%! q_source(?Dataset, ?Graph) is nondet.

q_source(Dataset) :-
  q_source_file(Dataset, _).


q_source(Dataset, Graph) :-
  q_source_file(Dataset, File),
  call_on_stream(File, q_source0(Graph)).


q_source0(Graph, _, Meta, Meta) :-
  Path = Meta.path,
  last(Path, Entry),
  Graph = Entry.name.



%! q_source_file(?Dataset, -File) is nondet.

q_source_file(Dataset, Path) :-
  absolute_file_name(source(.), Dir, [access(read),file_type(directory)]),
  (   var(Dataset)
  ->  directory_files(Dir, Files),
      member(File, Files),
      \+ is_dummy_file(File)
  ;   atomic_list_concat([Dataset,tar,gz], ., File)
  ),
  directory_file_path(Dir, File, Path).



%! q_source_ls is det.

q_source_ls :-
  q_ls0(q_source).





% SOURCE LAYER ↔ STORAGE LAYER

%! q_source2store(+Dataset) is det.
%! q_source2store(+Dataset, +Graph) is det.
%
% The resultant data file is called `<DATASET>_conv.<EXT>`.  The
% resultant VoID file, if any, is called `<NAME>_void.<EXT>`.
%
% The calls that are made are `call(<NAME>_load_data, +Sink, +G)`,
% `call(<NAME>_load_vocab, +Sink, +G)` and `call(<NAME>_load_void,
% +Sink, +G)`.

q_source2store(Dataset) :-
  forall(
    q_source(Dataset, Graph),
    q_source2store(Dataset, Graph)
  ).


q_source2store(Dataset, Graph) :-
  q_store(Dataset, Graph), !,
  debug(q(fs), "〈~a,~a〉 already in the Quine store.", [Dataset,Graph]).
q_source2store(Dataset, Graph) :-
  q_source_file(Dataset, File),
  q_graph0(Dataset, Graph, G),
  q_fs:q_source2store_hook(Dataset, File, Graph, G),
  debug(q(fs), "〈~a,~a〉 is added to the Quine store.", [Dataset,Graph]).



%! q_store_rm(+Dataset) is det.
%! q_store_rm(+Dataset, +Graph) is det.

q_store_rm(Dataset) :-
  q_store_dir(Dataset, Dir),
  delete_directory(Dir).


q_store_rm(Dataset, Graph) :-
  q_store_file(Dataset, Graph, File),
  delete_file(File).





% STORAGE LAYER %

%! q_store(?Dataset) is nondet.
%! q_store(?Dataset, ?Graph) is nondet.

q_store(Dataset) :-
  q_store_dir(Dataset, _).


q_store(Dataset, Graph) :-
  q_store_file(Dataset, Graph, _).



%! q_store_call(:Goal_2, +G) is det.
%! q_store_call(:Goal_2, +Dataset, +Graph) is det.

q_store_call(Goal_2, G) :-
  q_graph0(Dataset, Graph, G),
  q_store_call(Goal_2, Dataset, Graph).


q_store_call(Goal_2, Dataset, Graph) :-
  q_store_file(Dataset, Graph, File),
  call_to_ntriples(File, Goal_2, [entry(Graph)]).



%! q_store_dir(?Dataset, -Dir) is nondet.

q_store_dir(Dataset, Subdir) :-
  absolute_file_name(store(.), Dir, [access(read),file_type(directory)]),
  (   var(Dataset)
  ->  directory_files(Dir, Datasets),
      member(Dataset, Datasets),
      \+ is_dummy_file(Dataset)
  ;   true
  ),
  directory_file_path(Dir, Dataset, Subdir).



%! q_store_file(?Dataset, ?Graph, -File) is nondet.

q_store_file(Dataset, Graph, File) :-
  q_store(Dataset, Dir),
  (   var(Graph)
  ->  directory_files(Dir, Graphs),
      member(Graph, Graphs),
      \+ is_dummy_file(Graph)
  ;   true
  ),
  directory_file_path(Dir, Graph, Base),
  atomic_list_concat([Base,nt,gz], ., File).



%! q_store_ls is det.

q_store_ls :-
  q_ls0(q_store).





% STORAGE LAYER ↔ VIEWS LAYER %

%! q_2view(+M, +Dataset) is det.
%! q_2view(+M, +Dataset, +Graph) is det.

q_2view(M, Dataset) :-
  forall(
    q_store(Dataset, Graph),
    q_2view(M, Dataset, Graph)
  ).


q_2view(M, Dataset, Graph) :-
  q_store(Dataset, Graph), !,
  q_store2view(M, Dataset, Graph).
q_2view(M, Dataset, Graph) :-
  q_source(Dataset, Graph),
  q_source2store(Dataset, Graph),
  q_2view(M, Dataset, Graph).



%! q_store2view(+M, +Dataset) is det.
%! q_store2view(+M, +Dataset, +Graph) is det.
%
% Load 〈Dataset,Graph〉 into backend M.

q_store2view(M, Dataset) :-
  forall(
    q_store(Dataset, Graph, _),
    q_store2view(M, Dataset, Graph)
  ).


% N-Triples → HDT
q_store2view(hdt, Dataset, Graph) :-
  q_view_file(Dataset, Graph, [nt,gz], NTriplesFile),
  exists_file(NTriplesFile), !,
  q_view_file(Dataset, Graph, [hdt], HdtFile),
  hdt:hdt_create_from_file(HdtFile, NTriplesFile, []),
  debug(q(fs), "N-Triples → HDT", []).
% N-Quads → N-Triples
q_store2view(hdt, Dataset, Graph) :-
  q_view_file(Dataset, Graph, [nq,gz], NQuadsFile),
  exists_file(NQuadsFile), !,
  q_view_file(Dataset, Graph, [nt,gz], NTriplesFile),
  setup_call_cleanup(
    rdf_change_format(
      NQuadsFile,
      NTriplesFile,
      [from_format(nquads),to_format(ntriples)]
    ),
    (
      debug(q(fs), "N-Quads → N-Triples", []),
      q_store2view(hdt, Dataset, Graph)
    ),
    delete_file(NTriplesFile)
  ).
q_store2view(rdf, _, _).



%! q_view_rm(+M, +Dataset) is det.
%! q_view_rm(+M, +Dataset, +Graph) is det.

q_view_rm(M, Dataset) :-
  forall(
    q_view(M, Dataset, Graph),
    q_view_rm(M, Dataset, Graph)
  ).


q_view_rm(hdt, Dataset, Graph) :- !,
  q_view_file(Dataset, Graph, [hdt], HdtFile),
  (exists_file(HdtFile) -> delete_file(HdtFile) ; true),
  atomic_list_concat([HdtFile,index], ., HdtIndexFile),
  (exists_file(HdtIndexFile) -> delete_file(HdtIndexFile) ; true).
q_view_rm(rdf, _, _).





% VIEWS LAYER %

%! q_backend(?M) is nondet.
%
% Enumerate the currently supported backends.

q_backend(rdf).
q_backend(hdt).



%! q_change_view(+M1, +Dataset, +M2) is det.
%! q_change_view(+M1, +Dataset, +Graph, +M2) is det.

q_change_view(M1, Dataset, M2) :-
  forall(
    q_view(M1, Dataset, Graph, _),
    q_change_view(M1, Dataset, Graph, M2)
  ).


q_change_view(M1, Dataset, Graph, M2) :-
  with_mutex(q_fs, (
    q_rm_view(M1, Dataset, Graph),
    q_store2view(M2, Dataset, Graph)
  )).



%! q_view(?M, ?G) is nondet.
%! q_view(?M, ?Dataset, ?Graph) is nondet.

q_view(M, G) :-
  q_view(M, Dataset, Graph),
  q_graph0(Dataset, Graph, G).


q_view(hdt, Dataset, Graph) :- !,
  q_view_file(Dataset, Graph, [hdt], File),
  exists_file(File).
q_view(rdf, Dataset, Graph) :-
  q_store(Dataset, Graph).



%! q_view_file(+G, +Exts, -File) is det.
%! q_view_file(+Dataset, +Graph, +Exts, -File) is det.

q_view_file(G, Exts, File) :-
  q_graph0(Dataset, Graph, G),
  q_view_file(Dataset, Graph, Exts, File).


q_view_file(Dataset, Graph, Exts, File) :-
  absolute_file_name(views(.), Dir, [access(write),file_type(directory)]),
  directory_file_path(Dir, Dataset, Subdir),
  directory_file_path(Subdir, Graph, Base),
  atomic_list_concat(Exts, ., Ext),
  file_name_extension(Base, Ext, File).
  


%! q_view_ls is det.

q_view_ls :-
  q_ls0(q_view).





% VIEWS LAYER ↔ LOADED VIEW LAYER %

%! q_load(+M, +G) is det.
%! q_load(+M, +Dataset, +Graph) is det.

q_load(hdt, G) :-
  hdt_graph0(G, _, _), !.
q_load(hdt, G) :-
  q_view_file(G, [hdt], HdtFile),
  exists_file(HdtFile), !,
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph0(G, HdtFile, Hdt)),
  debug(q(fs), "HDT → open", []).
q_load(rdf, G) :-
  q_graph0(Dataset, Graph, G),
  q_store_file(Dataset, Graph, File),
  rdf11:rdf_load(File, [format(ntriples),graph(G)]).


q_load(M, Dataset, Graph) :-
  q_graph0(Dataset, Graph, G),
  q_load(M, G).



%! q_save(+M, +Dataset) is det.
%! q_save(+M, +Dataset, +Graph) is det.
%
% Save the contents of 〈Dataset,Graph〉 in backend M to the storage
% layer.

q_save(M, Dataset) :-
  forall(
    q_view(M, Dataset, Graph),
    q_save(M, Dataset, Graph)
  ).


q_save(hdt, Dataset, Graph) :-
  q_store_file(Dataset, Graph, NTriplesFile),
  q_graph0(Dataset, Graph, G),
  call_to_ntriples(NTriplesFile, q_hdt2stream0(G)).
q_save(rdf, Dataset, Graph) :-
  q_store(Dataset, Graph, File),
  q_view(Dataset, Graph, G),
  setup_call_cleanup(
    gzopen(File, write, Out),
    rdf_turtle_write:rdf_save_ntriples(stream(Out), [graph(G)]),
    close(Out)
  ).


q_hdt2stream0(G, State, Out) :-
  hdt(S, P, O, G),
  gen_ntuple(S, P, O, State, Out),
  fail.
q_hdt2stream0(_, _, _).



%! q_unload(+M, +G) is det.
%! q_unload(+M, +Dataset, +Graph) is det.

q_unload(hdt, G) :- !,
  with_mutex(q_fs, (
    hdt_graph0(G, _, Hdt),
    hdt:hdt_close(Hdt),
    retract(hdt_graph0(G,_,Hdt))
  )).
q_unload(rdf, G) :-
  rdf_unload_graph(G).


q_unload(M, Dataset, Graph) :-
  q_graph0(Dataset, Graph, G),
  q_unload(M, G).





% LOADED VIEW LAYER %

%! q_loaded(?M, ?G) is nondet.
%! q_loaded(?M, ?Dataset, ?Graph) is nondet.

q_loaded(hdt, G) :-
  hdt_graph(G, _).
q_loaded(rdf, G) :-
  rdf_graph(G).


q_loaded(M, Dataset, Graph) :-
  q_graph0(Dataset, Graph, G),
  q_loaded(M, G).



%! hdt_graph(?G, ?Hdt) is nondet.

hdt_graph(G, Hdt) :-
  hdt_graph0(G, _, Hdt).





% HELPERS %

%! source2store_options0(+Opts1, -Opts2) is det.
%
% The same code for option lists:
%
% ```prolog
% (option(alias(Alias), Opts1) -> true ; Alias = ex),
% merge_options(Opts1, [abox_alias(Alias),tbox_alias(Alias)], Opts2),
% ```

source2store_options0(Opts1, Opts3) :-
  del_dict(alias, Opts1, Alias, Opts2), !,
  Opts3 = Opts2.put(_{abox_alias: Alias, tbox_alias: Alias}).
source2store_options0(Opts1, Opts3) :-
  dict_put_def(abox_alias, Opts1, ex, Opts2),
  dict_put_def(tbox_alias, Opts2, ex, Opts3).



%! q_ls0(:Goal_2) is det.

q_ls0(Goal_2) :-
  findall(Dataset-Graph, call(Goal_2, Dataset, Graph, _), Pairs),
  pairs_to_tree(Pairs, Tree),
  print_tree(Tree).



%! q_graph0(+Dataset, +Graph, -G) is det.
%! q_graph0(-Dataset, -Graph, +G) is det.

q_graph0(Dataset, Graph, G) :-
  nonvar(G), !,
  rdf_global_id(triply:Local, G),
  atomic_list_concat([Dataset,Graph], '_', Local).
q_graph0(Dataset, Graph, G) :-
  nonvar(Dataset),
  nonvar(Graph), !,
  atomic_list_concat([Dataset,Graph], '_', Local),
  rdf_global_id(triply:Local, G).
