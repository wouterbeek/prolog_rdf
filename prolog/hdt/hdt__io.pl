:- module(
  hdt__io,
  [
    hdt__call/2,    % :Goal_2, +G
    hdt__call/3,    % :Goal_2, +G, +Opts
    hdt__delete/1,  % +G
    hdt__graph/1,   % ?G
    hdt__graph/2,   % ?G, -Hdt
    hdt__load/1,    % +G
    hdt__refresh/1, % +G
    hdt__unload/1,  % +G
    hdt2rdf/1       % +G
  ]
).

/** <module> HDT extensions

@author Wouter Beek
@version 2016/04-2016/07
*/

:- use_module(library(apply)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(hdt), []).
:- use_module(library(os/io)).
:- use_module(library(q/q__io)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

%! hdt_graph0(?G, ?Hdt, ?HdtFile, ?NTriplesFile) is nondet.

:- dynamic
    hdt_graph0/4.

:- meta_predicate
    hdt__call(2, +),
    hdt__call(2, +, +).

:- rdf_meta
   hdt__call(:, r),
   hdt__call(:, r, +),
   hdt__delete(r),
   hdt__graph(r),
   hdt__graph(r, ?),
   hdt__load(r),
   hdt__refresh(r),
   hdt__unload(r),
   hdt2rdf(r).





%! hdt__call(:Goal_1, +G) is det.
%! hdt__call(:Goal_1, +G, +Opts) is det.
%
% The following call is made: `call(Goal_2, State, Out)`, where State
% maintains the state during N-Triple writing and Out is the output
% stream.

hdt__call(Goal_2, G) :-
  hdt__call(Goal_2, G, []).


hdt__call(Goal_2, G, Opts) :-
  q_graph_to_file(G, [nt,gz], File),
  call_to_ntriples(File, Goal_2, Opts).



%! hdt__delete(+G) is det.
%
% Deletes the HDT files, if any, related to graph G from disk.

hdt__delete(G) :-
  q_graph_to_file(G, [hdt], HdtFile),
  (exists_file(HdtFile) -> delete_file(HdtFile) ; true),
  atomic_list_concat([HdtFile,index], ., HdtIndexFile),
  (exists_file(HdtIndexFile) -> delete_file(HdtIndexFile) ; true).



%! hdt__graph(?G) is nondet.
%! hdt__graph(?G, ?Hdt) is nondet.
%
% Enumerates the graphs that are currently loaded from disk.

hdt__graph(G) :-
  hdt__graph(G, _).


hdt__graph(G, Hdt) :-
  hdt_graph0(G, Hdt, _, _).



%! hdt__load(+G) is det.
%
% Loads graph G from disk.
%
% Options are passed to hdt_create_from_file/3.

hdt__load(G) :-
  q_graph_to_file(G, [hdt], HdtFile),
  hdt__load(HdtFile, _, G).


% Already opened
hdt__load(_, _, G) :-
  hdt__graph(G), !.
% HDT → open
hdt__load(HdtFile, NTriplesFile, G) :-
  exists_file(HdtFile), !,
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph0(G, Hdt, HdtFile, NTriplesFile)),
  debug(q(ext), "HDT → open", []).
% N-Triples → HDT
hdt__load(HdtFile, NTriplesFile, G) :-
  file_name_extension(Base, hdt, HdtFile),
  file_name_extension(Base, 'nt.gz', NTriplesFile),
  exists_file(NTriplesFile), !,
  hdt:hdt_create_from_file(HdtFile, NTriplesFile, []),
  debug(q(ext), "N-Triples → HDT", []),
  hdt__load(HdtFile, NTriplesFile, G).
% N-Quads → N-Triples
hdt__load(HdtFile, NTriplesFile, G) :-
  file_name_extension(Base, hdt, HdtFile),
  file_name_extension(Base, 'nq.gz', NQuadsFile),
  exists_file(NQuadsFile), !,
  file_name_extension(Base, 'nt.gz', NTriplesFile),
  setup_call_cleanup(
    rdf_change_format(
      NQuadsFile,
      NTriplesFile,
      [from_format(nquads),to_format(ntriples)]
    ),
    (
      debug(q(ext), "N-Quads → N-Triples", []),
      hdt__load(HdtFile, NTriplesFile, G)
    ),
    delete_file(NTriplesFile)
  ).



%! hdt__refresh(+G) is det.
%
% Reloads graph G from disk.  This is typically done affter triples
% are asserted to G using hdt_assert/4 and the newly added triples
% need to be queried.

hdt__refresh(G) :-
  q_graph_to_file(G, [hdt], HdtFile),
  hdt__unload(G),
  hdt__delete(G),
  hdt__load(HdtFile, G).



%! hdt__unload(+G) is det.
%
% Unload graph G if it is currently accessible from disk.

hdt__unload(G) :-
  hdt__graph(G, Hdt),
  hdt:hdt_close(Hdt),
  retract(hdt_graph0(G,Hdt,_,_)).



%! hdt2rdf(+G) is det.
%
% Load graph G, which is already disk-accessible, into memory.
%
% Switching from disk-access to memory-access is done by calling
% `hdt2rdf(G), hdt__unload(G)`.

hdt2rdf(G) :-
  hdt(S, P, O, G),
  rdf_assert(S, P, O, G),
  fail.
hdt2rdf(_).
