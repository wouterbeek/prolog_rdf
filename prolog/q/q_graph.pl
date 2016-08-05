:- module(
  q_graph,
  [
  % SOURCE
  q_source_file/1,    % -File

    % SOURCE ⬄ STORE
    q_source2store/0,
    q_source2store/2, % +File, -G

  % STORE
  q_store_file/2,     % -File, ?G
  q_store_graph/1,    % -G
    
    % STORE ⬄ VIEW
    q_store2view/1,   % +M
    q_store2view/2,   % +M, +G

  % VIEW
  q_view_file/3,      % -M, -File, -G
  q_view_graph/2,     % -M, -G
    
    % VIEW ⬄ LOADED
    q_load/1,         % +M
    q_load/2,         % +M, +G
    q_save/1,         % +M
    q_save/2,         % +M, +G
    q_save_append/1,  % +M
    q_save_append/2,  % +M, +G
    q_unload/1,       % +M
    q_unload/2,       % +M, +G

  % LOADED
  q_graph/2,          % ?M, ?G

  % GENERICS
  q_graph_iri/2,      % +Name, -G
  q_ls/0,
  q_ls/1              % +Type
  ]
).

/** <module> Quine graph store

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(os/directory_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_print)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_file)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(tree/s_tree)).


%! hdt_graph0(?G, ?HdtFile, ?Hdt) is nondet.

:- dynamic
    hdt_graph0/3.


:- meta_predicate
    q_ls(2).


%! q_source2store_hook(+Format, +Source, +Sink, +Opts) is det.
%! q_source_extensions_hook(?Format, ?Exts) is nondet.
%! q_store_extensions_hook(?Format, ?Exts) is nondet.
%! q_view_extensions_hook(?Format, ?Exts) is nondet.

:- multifile
    q_source2store_hook/4,
    q_source_extensions_hook/2,
    q_view_extensions_hook/2.

q_source2store_hook(rdf, Source, Sink, Opts1) :- !,
  dict_options(Opts1, Opts2),
  rdf_change_format(Source, Sink, Opts2).


:- rdf_meta
   q_graph(?, r),
   q_load(+, r),
   q_save(+, r),
   q_save_append(+, r),
   q_store2view(+, r),
   q_store_file(?, r),
   q_store_graph(r),
   q_unload(+, r),
   q_view_graph(-, r),
   q_view_file(-, -, r).





% SOURCE %

%! q_source_extensions(?Fromat, ?Exts) is nondet.

q_source_extensions(rdf, [Ext]) :-
  rdf_default_file_extension(_, Ext).
q_source_extensions(Format, Exts) :-
  q_source_extensions_hook(Format, Exts).



%! q_source_file(-File) is nondet.
%
% Enumerates the files currently in the source root directory.

q_source_file(File) :-
  q_root(source, Root),
  directory_path(Root, File).





% SOURCE ⬄ STORE %

%! q_source2store is det.
%! q_source2store(+Source, -Sink) is det.
%
% Convert Source into an RDF file with graph G.  Source is one of the
% following:
%
%   - file(+File:atom, +Opts:dict)
%
%   - url(+Url:atom, +Opts:dict)
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
    q_source2store(file(File,_{}), _)
  ).


q_source2store(file(Source,Opts), Sink) :- !,
  access_file(Source, read),
  file_extensions(Source, Exts),
  q_source_extensions(Format, Exts),
  q_file_to_graph(Source, G),
  q_graph_to_file(store, G, ntriples, Sink),
  once(q_source_extensions(Format, Exts)),
  q_source2store_hook(Format, Source, Sink, Opts).
q_source2store(url(Source,Opts), Sink) :-
  q_graph_iri(Opts.name, G),
  q_graph_to_file(store, G, ntriples, Sink),
  file_extensions(Source, Exts),
  once(q_source_extensions(Format, Exts)),
  q_source2store_hook(Format, Source, Sink, Opts).





% STORE %

%! q_store_extensions(?Format, ?Exts) is nondet.

q_store_extensions(hdt, [hdt]).
q_store_extensions(nquads, [nq,gz]).
q_store_extensions(ntriples, [nt,gz]).



%! q_store_file(-File, -G) is nondet
%
% Enumerates the files and associated graph names that are currently
% in the store directory.

q_store_file(File, G) :-
  q_root(store, Root),
  directory_file(Root, File),
  q_file_to_graph(File, G).



%! q_store_graph(-G) is nondet

q_store_graph(G) :-
  q_store_file(_, G).





% STORE ⬄ VIEW %

%! q_store2view(+M) is det.
%! q_store2view(+M, +G) is det.
%
% Create a view of store graph G in backend M.

q_store2view(M) :-
  forall(
    q_store_graph(G),
    q_store2view(M, G)
  ).


% Nothing to do: view already exists.
q_store2view(M, G) :-
  q_graph_to_file(view, G, M, File),
  exists_file(File), !.
% N-Triples → HDT
q_store2view(hdt, G) :-
  q_graph_to_file(store, G, ntriples, NTriplesFile),
  exists_file(NTriplesFile), !,
  q_graph_to_file(view, G, hdt, HdtFile),
  hdt:hdt_create_from_file(HdtFile, NTriplesFile, []),
  indent_debug(q(q_graph), "N-Triples → HDT").
% N-Quads → N-Triples
q_store2view(hdt, G) :-
  q_graph_to_file(store, G, nquads, NQuadsFile),
  exists_file(NQuadsFile), !,
  q_graph_to_file(store, G, ntriples, NTriplesFile),
  setup_call_cleanup(
    rdf_change_format(
      NQuadsFile,
      NTriplesFile,
      [from_format(nquads),to_format(ntriples)]
    ),
    (
      indent_debug(q(q_graph), "N-Quads → N-Triples"),
      q_store2view(hdt, G)
    ),
    delete_file(NTriplesFile)
  ).
q_store2view(rdf, G) :-
  q_graph_to_file(store, G, ntriples, File),
  q_root(view, Root),
  create_file_link(File, Root).





% VIEW %

%! q_view_extensions(?Format, ?Exts) is nondet.

q_view_extensions(hdt, [hdt]).
q_view_extensions(rdf, [nt,gz]).
q_view_extensions(Format, Exts) :-
  q_view_extensions_hook(Format, Exts).



%! q_view_file(-M, -File, -G) is nondet.
%
% Enumerates the files and associated graph names that are currently
% in the store directory.

q_view_file(M, File, G) :-
  q_root(view, Root),
  directory_file(Root, File),
  q_file_to_graph(File, G),
  file_extensions(File, Exts),
  once(q_view_extensions(M, Exts)).



%! q_view_graph(-M, -G) is nondet.

q_view_graph(M, G) :-
  q_view_file(M, _, G).





% VIEW ⬄ LOADED %

%! q_load(+M) is det.
%! q_load(+M, +G) is det.
%
% Load graph G into backend M.

q_load(M) :-
  forall(
    q_view_graph(M, G),
    q_load(M, G)
  ).


% Nothing to do.
q_load(M, G) :-
  q_graph(M, G), !.
% Cannot proceed: create the view first.
q_load(M, G) :-
  \+ q_view_graph(M, G), !,
  q_store2view(M, G),
  q_load(M, G).
% Load HDT view.
q_load(hdt, G) :-
  q_graph_to_file(view, G, hdt, HdtFile),
  exists_file(HdtFile), !,
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph0(G, HdtFile, Hdt)),
  indent_debug(q(q_graph), "HDT → open").
% Load RDF view.
q_load(rdf, G) :-
  q_graph_to_file(view, G, ntriples, NTriplesFile),
  rdf_load(NTriplesFile, [format(ntriples),graph(G)]).



%! q_save(+M) is det.
%! q_save(+M, +G) is det.

q_save(M) :-
  q_save0(M, [mode(write)]).


q_save(M, G) :-
  q_save0(M, G, [mode(write)]).



%! q_save_append(+M) is det.
%! q_save_append(+M, +G) is det.
%
% Save the contents of 〈Dataset,Graph〉 in backend M to the storage
% layer.

q_save_append(M) :-
  q_save0(M, []).


q_save_append(M, G) :-
  q_save0(M, G, []).


q_save0(M, Opts) :-
  forall(
    q_graph(M, G),
    q_save0(M, G, Opts)
  ).


q_save0(M, G, Opts1) :-
  q_graph_to_file(store, G, ntriples, NTriplesFile),
  merge_options(Opts1, [rdf_format(ntriples)], Opts2),
  rdf_write_to_sink(NTriplesFile, M, G, Opts2).



%! q_unload(+M) is det.
%! q_unload(+M, +G) is det.

q_unload(M) :-
  forall(
    q_graph(M, G),
    q_unload(M, G)
  ).


q_unload(hdt, G) :- !,
  with_mutex(q_io, (
    hdt_graph0(G, _, Hdt),
    hdt:hdt_close(Hdt),
    retract(hdt_graph0(G,_,Hdt))
  )).
q_unload(rdf, G) :-
  rdf_unload_graph(G).





% LOADED %

%! q_graph(+M, +G) is semidet.
%! q_graph(+M, -G) is nondet.
%! q_graph(-M, +G) is nondet.
%! q_graph(-M, -G) is nondet.

q_graph(hdt, G) :-
  hdt_graph0(G, _, _).
q_graph(rdf, G) :-
  rdf_graph(G).





% GENERICS %

%! q_graph_iri(+Ref, -G) is det.

q_graph_iri(Ref, G) :-
  q_abox_iri(ns, graph, Ref, G).



%! q_ls is det.
%! q_ls(+Type) is det.

q_ls :-
  q_ls(q_source_graph0),
  q_ls(q_store_graph0),
  q_ls(q_view_graph0),
  q_ls(q_graph0).

q_source_graph0(source, File) :-
  q_source_file(File).

q_store_graph0(store, G) :-
  q_store_graph(G).

q_view_graph0(view(M), G) :-
  q_view_graph(M, G).

q_graph0(loaded(M), G) :-
  q_graph(M, G).





% HELPERS %

%! q_file_to_graph(+File, -G) is det.

q_file_to_graph(File, G) :-
  file_name_extension(Base, _, File),
  directory_file_path(_, Local, Base),
  q_graph_iri(Local, G).



%! q_graph_to_base(+Type, +G, -Base) is det.

q_graph_to_base(Type, G, Base) :-
  q_root(Type, Root),
  uri_components(G, Comps),
  uri_data(path, Comps, Path),
  atomic_list_concat(['',id,graph,Local], /, Path),
  directory_file_path(Root, Local, Base).



%! q_graph_to_file(+Type, +G, +Format, -File) is det.

q_graph_to_file(Type, G, Format, File) :-
  q_graph_to_base(Type, G, Base),
  once((
      q_source_extensions(Format, Exts)
  ;   q_store_extensions(Format, Exts)
  ;   q_view_extensions(Format, Exts)
  )),
  atomic_list_concat([Base|Exts], ., File).



%! q_ls(:Goal_2) is det.

q_ls(Goal_2) :-
  aggregate_all(
    set(X-Y),
    (
      call(Goal_2, D, G),
      (X = D, Y = G ; X = root, Y = D)
    ),
    Pairs
  ),
  (   pairs_to_tree(Pairs, Tree)
  ->  print_tree(Tree, [label_writer(q_print:dcg_q_print_something)])
  ;   writeln("∅")
  ).



%! q_root(+Type, -Root) is det.

q_root(Type, Root) :-
  Type =.. [Type0|_],
  Spec =.. [Type0,.],
  absolute_file_name(Spec, Root, [access(write),file_type(directory)]).
