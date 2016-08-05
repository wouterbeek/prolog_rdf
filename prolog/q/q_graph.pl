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
    q_view2loaded/1,  % +M
    q_view2loaded/2,  % +M, +G

  % LOADED
  q_graph/2,          % ?M, ?G

  % GENERICS
  q_ls/0,
  q_ls/1,             % +Type
  q_name_to_graph/2   % +Name, -G
  ]
).

/** <module> Quine graph store

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(os/directory_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(q/q_print)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_file)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(tree/s_tree)).


%! hdt_graph0(?G, ?HdtFile, ?File) is nondet.

:- dynamic
    hdt_graph0/3.


:- meta_predicate
    q_ls(2).


%! q_source2store_hook(+Format, +Source, +Sink, +Opts) is det.
%! q_source_extensions(?Format, ?Exts) is nondet.
%! q_store_extensions(?Format, ?Exts) is nondet.
%! q_view_extensions(?Format, ?Exts) is nondet.

:- multifile
    q_source2store_hook/4,
    q_source_extensions/2,
    q_store_extensions/2,
    q_view_extensions/2.

q_source2store_hook(rdf, Source, Sink, Opts) :- !,
  rdf_change_format(Source, Sink, Opts).

q_source_extensions(rdf, [Ext]) :-
  rdf_default_file_extension(_, Ext).

q_store_extensions(hdt,      [hdt]  ).
q_store_extensions(nquads,   [nq,gz]).
q_store_extensions(ntriples, [nt,gz]).

q_view_extensions(hdt, [hdt]).
q_view_extensions(rdf, [nt,gz]).


:- rdf_meta
   q_graph(?, r),
   q_load(+, r),
   q_store2view(+, r),
   q_store_file(?, r),
   q_store_graph(r),
   q_view2loaded(+, r),
   q_view_graph(-, r),
   q_view_file(-, -, r).





% SOURCE %

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
  q_name_to_graph(Opts.name, G),
  q_graph_to_file(store, G, ntriples, Sink),
  file_extensions(Source, Exts),
  once(q_source_extensions(Format, Exts)),
  q_source2store_hook(Format, Source, Sink, Opts).





% STORE %

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

%! q_view2loaded(+M) is det.
%! q_view2loaded(+M, +G) is det.
%
% Load graph G into backend M.

q_view2loaded(M) :-
  forall(
    q_view_graph(M, G),
    q_view2loaded(M, G)
  ).


q_view2loaded(M, G) :-
  q_graph(M, G).
q_view2loaded(hdt, G) :-
  q_graph_to_file(view, G, hdt, HdtFile),
  exists_file(HdtFile), !,
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph0(G, HdtFile, Hdt)),
  indent_debug(q(q_graph), "HDT → open").
q_load(rdf, G) :-
  q_graph_to_file_name(store, G, ntriples, NTriplesFile),
  rdf_load(NTriplesFile, [format(ntriples),graph(G)]).




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



%! q_name_to_graph(+Name, -G) is det.

q_name_to_graph(Name, G) :-
  qb_abox_iri(ns, graph, Name, G).





% HELPERS %

%! q_file_to_graph(+File, -G) is det.

q_file_to_graph(File, G) :-
  file_name_extension(Base, _, File),
  directory_file_path(_, Local, Base),
  q_name_to_graph(Local, G).



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
