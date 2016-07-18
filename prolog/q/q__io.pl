:- module(
  q__io,
  [
    q_aggregate_all/3, % +Template, :Goal_0, -Result
    q_available/1,     % -G
    q_backend/1,       % ?M
    q_exists/1,        % +G
    q_fs/0,
    q_graph/1,         % ?G
    q_graph/2,         % ?M, ?G
    q_graph_to_file/3, % +G, +Exts, -File
    q_load/2,          % +M, +G
    q_load_or_call/3,  % +M, :Goal_1, +G
    q_member/2,        % ?Elem, +L
    q_save/1,          % +G
    q_snap/1,          % :Goal_0
   %q_transaction/1,   % :Goal_0
   %q_transaction/2,   % :Goal_0, +Id
   %q_transaction/3,   % :Goal_0, +Is, +Opts
    q_unload/1,        % +G
    q_unload/2         % +M, +G
  ]
).
:- reexport(library(semweb/rdf11), [
     rdf_transaction/1 as q_transaction,
     rdf_transaction/2 as q_transaction,
     rdf_transaction/3 as q_transaction
   ]).

/** <module> Quine I/O

| **File name**           | **Graph name**    |
|:------------------------|:------------------|
| `<ALIAS>_<LOCAL>.nq.gz` | `<ALIAS>:<LOCAL>` |

@author Wouter Beek
@version 2016/06-2016/07
*/

:- use_module(library(debug)).
:- use_module(library(hdt/hdt__io)).
:- use_module(library(lists)).
:- use_module(library(q/q_term)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    q_aggregate_all(+, 0, -),
    q_load_or_call(+, 1, +),
    q_snap(0).

:- rdf_meta
   q_aggregate_all(+, t, -),
   q_exists(r),
   q_graph(?, r),
   q_load(+, r),
   q_load_or_call(+, :, r),
   q_member(r, t),
   q_save(r),
   q_unload(r),
   q_unload(+, r).

:- debug(q(io)).





%! q_aggregate_all(+Template, :Goal_0, -Result) is det.
%
% Calls aggregate_all/3 under RDF alias expansion.

q_aggregate_all(Template, Goal_0, Result) :-
  aggregate_all(Template, Goal_0, Result).



%! q_available(-G) is nondet.
%
% Enumerates the _available_ graphs G that are not currently loaded
% into a backend.

q_available(G) :-
  distinct(G, (
    absolute_file_name(data(.), Dir, [access(read),file_type(directory)]),
    directory_file_path(Dir, '*.nq.gz', Wildcard),
    expand_file_name(Wildcard, Paths),
    member(Path, Paths),
    directory_file_path(Dir, File, Path),
    atomic_list_concat([Alias|Locals1], '_', File),
    append(Locals0, [Last1], Locals1),
    atomic_list_concat([Last2|_], ., Last1),
    append(Locals0, [Last2], Locals2),
    atomic_list_concat(Locals2, '_', Local),
    rdf_global_id(Alias:Local, G),
    \+ q_graph(G)
  )).



%! q_backend(?M) is nondet.
%
% Enumerate the currently supported backends.

q_backend(rdf).
q_backend(hdt).



%! q_exists(+G) is semidet.
%
% Succeeds if graph G is available in the data store.

q_exists(G) :-
  q_graph_to_file(G, [nt,gz], File),
  exists_file(File).



%! q_fs is det.
%
% Print the contents of the Quine filesystem to user output.

q_fs :-
  q_fs('*.hdt'),
  q_fs('*.nt.gz'),
  q_fs('*.nq.gz').


q_fs(Wildcard) :-
  Out = user_output,
  q_fs(Wildcard, Files),
  (   Files == []
  ->  true
  ;   format(Out, "~w:~n", [Wildcard]),
      maplist({Out}/[File]>>format(Out, "  ▻ ~w~n", [File]), Files)
  ).


q_fs(Wildcard0, Files) :-
  absolute_file_name(data(.), Dir, [access(read),file_type(directory)]),
  directory_file_path(Dir, Wildcard0, Wildcard),
  expand_file_name(Wildcard, Paths),
  maplist(directory_file_path(Dir), Files, Paths).



%! q_graph(?G) is nondet.
%! q_graph(?M, ?G) is nondet.
%
% Enumerates the _loaded_ graphs G, optionally with the backend M in
% which it is loaded.

q_graph(G) :-
  q_graph(_, G).


q_graph(rdf, G) :-
  rdf11:rdf_graph(G).
q_graph(hdt, G) :-
  hdt__graph(G).



%! q_graph_to_file(+G, +Exts, -File) is det.
%
% Translates graph G to its corresponding File with file extensions
% Exts.

q_graph_to_file(G, Exts, File) :-
  q_graph_to_base(G, Base),
  atomic_list_concat([Base|Exts], ., Local),
  absolute_file_name(data(Local), File, [access(write)]).



%! q_load(+M, +G) is det.
%
% Load graph G into backend M.

q_load(hdt, G) :- !,
  hdt__load(G).
q_load(rdf, G) :- !,
  rdf__load(G).



%! q_load_or_call(+M, :Goal_1, +G) is det.
%
% Load graph G into backend M or, if graph G is not currently in the
% data store, call 〈Goal_1,G〉 that generates and then loads graph G.

q_load_or_call(M, Goal_1, G) :-
  (   q_load(M, G)
  ->  debug(q(io), "Loaded graph ~w into backend ~a", [G,M])
  ;   call(Goal_1, G),
      debug(q(io), "Called goal to generate graph ~w", [G]),
      q_load_or_call(M, Goal_1, G)
  ).



%! q_member(?Elem, +L) is nondet.
%
% Calls member/2 under RDF alias expansion.

q_member(Elem, L) :-
  member(Elem, L).



%! q_save(+G) is det.
%
% Save graph G to its corresponding file.

q_save(G) :-
  rdf__save(G).



%! q_snap(:Goal_0) .
%
% Call Goal_0 inside an RDF snapshot.

q_snap(Goal_0) :-
  q_transaction(Goal_0, _, [snapshot(true)]).



%! q_unload(+G) is det.
%! q_unload(+M, +G) is det.
%
% Unload graph G from backend M or from all backends.

q_unload(G) :-
  forall(q_graph(M, G), q_unload(M, G)).


q_unload(rdf, G) :- !,
  rdf_unload_graph(G).
q_unload(hdt, G) :- !,
  hdt__unload(G).





% HELPERS %

%! q_graph_to_base(+G, -Base) is det.
%
% Translates Quine graph names to base IRIs.

q_graph_to_base(G, Base) :-
  rdf_global_id(Alias:Local, G), !,
  atomic_list_concat([Alias,Local], '_', Base).
q_graph_to_base(G, Base) :-
  Base = G.
