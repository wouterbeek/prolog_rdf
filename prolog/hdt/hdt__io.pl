:- module(
  hdt__io,
  [
    hdt__call/3,    % +Mode, :Goal_1, +G
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
@version 2016/04-2016/06
*/

:- use_module(library(apply)).
:- use_module(library(hdt), []).
:- use_module(library(os/open_any2)).
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
    hdt__call(+, 1, +).

:- rdf_meta
   hdt__call(+, :, r),
   hdt__delete(r),
   hdt__graph(r),
   hdt__graph(r, ?),
   hdt__load(r),
   hdt__refresh(r),
   hdt__unload(r),
   hdt2rdf(r).





%! hdt__call(+Mode, :Goal_1, +G) is det.

hdt__call(Mode, Goal_1, G) :- !,
  q_graph_to_file(G, [nt,gz], File),
  setup_call_cleanup(
    gzopen(File, Mode, Out),
    call(Goal_1, Out),
    close(Out)
  ).



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
    ensure_ntriples(NQuadsFile, NTriplesFile),
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





% HELPERS %

%! ensure_ntriples(+NQuadsFile, +NTriplesFile) is det.

ensure_ntriples(From, To) :-
  Opts = [rdf_format(ntriples)],
  setup_call_cleanup(
    (
      gzopen(To, write, Sink),
      gen_ntuples:gen_ntuples_begin(State, Opts)
    ),
    with_output_to(
      Sink, 
      rdf_call_on_tuples(
        From,
        {State}/[_,S,P,O,G]>>gen_ntuple(State, S, P, O, G)
      )
    ),
    (
      gen_ntuples:gen_ntuples_end(State, Opts),
      close(Sink)
    )
  ).
