:- module(
  q__io,
  [
    q_aggregate_all/3, % +Template, :Goal_0, -Result
    q_available/2,     % ?M, -G
    q_backend/1,       % ?M
    q_exists/1,        % +G
    q_fs/0,
    q_graph/2,         % ?M, ?G
    q_graph_to_file/3, % +G, +Comps, -File
    q_load/2,          % +M, +G
    q_load_or_call/3,  % +M, :Goal_1, +G
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

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(debug)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(hdt/hdt__io)).
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
   q_save(r),
   q_unload(r),
   q_unload(+, r).

:- debug(q(io)).





%! q_aggregate_all(+Template, :Goal_0, -Result) is det.

q_aggregate_all(Template, Goal_0, Result) :-
  aggregate_all(Template, Goal_0, Result).



%! q_available(?M, -G) is nondet.

q_available(M, G) :-
  distinct(G, (
    member(Wildcard0, ['*.hdt','*.nt.gz','*.nq.gz']),
    absolute_file_name(data(.), Dir, [access(read),file_type(directory)]),
    directory_file_path(Dir, Wildcard0, Wildcard),
    expand_file_name(Wildcard, Paths),
    member(Path, Paths),
    directory_file_path(Dir, File, Path),
    atomic_list_concat([Base|_], ., File),
    atomic_list_concat([Alias,Local], '_', Base),
    rdf_global_id(Alias:Local, G),
    \+ q_graph(M, G)
  )).



%! q_backend(?M) is nondet.

q_backend(rdf).
q_backend(hdt).



%! q_exists(+G) is semidet.
%
% Succeeds if graph G is available in the data store.

q_exists(G) :-
  q_graph_to_file(G, [nt,gz], File),
  exists_file(File).



%! q_fs is det.

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



%! q_graph(?M, ?G) is nondet.

q_graph(rdf, G) :-
  rdf11:rdf_graph(G).
q_graph(hdt, G) :-
  hdt__graph(G).



%! q_graph_to_file(+G, +Comps, -HdtFile) is det.

q_graph_to_file(G, Comps, HdtFile) :-
  q_graph_to_base(G, Base),
  atomic_list_concat([Base|Comps], ., Local),
  absolute_file_name(data(Local), HdtFile, [access(write)]).



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
% data store, call a goal that generates and then loads graph G.

q_load_or_call(M, Goal_1, G) :-
  (   q_load(M, G)
  ->  debug(q(io), "Loaded graph ~w into backend ~a", [G,M])
  ;   call(Goal_1, G),
      debug(q(io), "Called goal to generate graph ~w", [G]),
      q_load_or_call(M, Goal_1, G)
  ).



%! q_save(+G) is det.

q_save(G) :-
  rdf__save(G).



%! q_snap(:Goal_0) .

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

q_graph_to_base(G, Base) :-
  rdf_global_id(Alias:Local, G), !,
  atomic_list_concat([Alias,Local], '_', Base).
q_graph_to_base(G, Base) :-
  Base = G.
