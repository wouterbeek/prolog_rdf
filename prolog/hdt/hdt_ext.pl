:- module(
  hdt_ext,
  [
    hdt/3,                % ?S, ?P, ?O
    hdt/4,                % ?S, ?P, ?O, ?G
    hdt_assert/1,         % +Tuple
    hdt_assert/2,         % +Triple, +G
    hdt_assert/3,         % +S, +P, +O
    hdt_assert/4,         % +S, +P, +O, +G
    hdt_delete/1,         % +G
    hdt_fs/0,
    hdt_graph/1,          % ?G
    hdt_graph/2,          % ?G, ?Hdt
    hdt_header/4,         % ?S, ?P, ?O, +G
    hdt_load/1,           % +G
    hdt_load/2,           % +G, +Opts
    hdt_load_to_memory/1, % +G
    hdt_refresh/1,        % +G
    hdt_unload/1          % +G
  ]
).

/** <module> HDT extensions

@author Wouter Beek
@version 2016/04-2016/06
*/

:- use_module(library(apply)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(hdt), []).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).
:- use_module(library(z/z_ext)).
:- use_module(library(zlib)).

%! hdt_graph(?G, ?Hdt, ?HdtFile, ?NTriplesFile) is nondet.

:- dynamic
    hdt_graph/4.

:- rdf_meta
   hdt(r, r, o),
   hdt(r, r, o, r),
   hdt_assert(t),
   hdt_assert(t, r),
   hdt_assert(r, r, o),
   hdt_assert(r, r, o, r),
   hdt_delete(r),
   hdt_graph(r),
   hdt_graph(r, ?),
   hdt_header(r, r, o, r),
   hdt_load(r),
   hdt_load(r, +),
   hdt_load_to_memory(r),
   hdt_refresh(r),
   hdt_unload(r).





%! hdt(?S, ?P, ?O) is nondet.
%! hdt(?S, ?P, ?O, ?G) is nondet.

hdt(S, P, O) :-
  distinct(rdf(S,P,O), hdt(S, P, O, _)).


hdt(S, P, O, G) :-
  hdt_graph(G, Hdt),
  hdt:hdt_search(Hdt, S, P, O).



%! hdt_assert(+Tuple) is det.
%! hdt_assert(+Triple, +G) is det.
%! hdt_assert(+S, +P, +O) is det.
%! hdt_assert(+S, +P, +O, +G) is det.

hdt_assert(rdf(S,P,O)) :- !,
  hdt_assert(S, P, O).
hdt_assert(rdf(S,P,O,G)) :-
  hdt_assert(S, P, O, G).


hdt_assert(rdf(S,P,O), G) :-
  hdt_assert(S, P, O, G).


hdt_assert(S, P, O) :-
  rdf_default_graph(G),
  hdt_assert(S, P, O, G).


hdt_assert(S, P, O, G) :-
  z_graph_to_file(G, [nt,gz], NTriplesFile),
  call_to_stream(
    NTriplesFile,
    {S,P,O}/[Out,M,M]>>with_output_to(Out, gen_ntriple(S, P, O)),
    [compression(gzip)]
  ).



%! hdt_delete(+G) is det.

hdt_delete(G) :-
  z_graph_to_file(G, [hdt], HdtFile),
  (exists_file(HdtFile) -> delete_file(HdtFile) ; true),
  atomic_list_concat([HdtFile,index], ., HdtIndexFile),
  (exists_file(HdtIndexFile) -> delete_file(HdtIndexFile) ; true).



%! hdt_fs is det.

hdt_fs :-
  hdt_fs('*.hdt'),
  hdt_fs('*.nt.gz'),
  hdt_fs('*.nq.gz').


hdt_fs(Wildcard) :-
  Out = user_output,
  hdt_fs(Wildcard, Files),
  (   Files == []
  ->  true
  ;   format(Out, "~w:~n", [Wildcard]),
      maplist({Out}/[File]>>format(Out, "  ▻ ~w~n", [File]), Files)
  ).


hdt_fs(Wildcard0, Files) :-
  absolute_file_name(data(.), Dir, [access(read),file_type(directory)]),
  directory_file_path(Dir, Wildcard0, Wildcard),
  expand_file_name(Wildcard, Paths),
  maplist(directory_file_path(Dir), Files, Paths).



%! hdt_graph(?G) is nondet.
%! hdt_graph(?G, ?Hdt) is nondet.

hdt_graph(G) :-
  hdt_graph(G, _).


hdt_graph(G, Hdt):-
  hdt_graph(G, Hdt, _, _).



%! hdt_header(?S, ?P, ?O, ?G) is nondet.
%
% The following predicates are supported:
%
%   * `'<http://rdfs.org/ns/void#triples>'` with object `N^^xsd:integer`

hdt_header(S, P, O, G) :-
  hdt_graph(G, Hdt),
  hdt:hdt_header(Hdt, S, P, O).



%! hdt_load(+G) is det.
%! hdt_load(+G, +Opts) is det.
%
% Options are passed to hdt_create_from_file/3.

hdt_load(G) :-
  hdt_load(G, []).


hdt_load(G, Opts) :-
  z_graph_to_file(G, [hdt], HdtFile),
  hdt_load(HdtFile, _, G, Opts).


% Already opened
hdt_load(_, _, G, _) :-
  hdt_graph(G, _), !.
% HDT → open
hdt_load(HdtFile, NTriplesFile, G, _) :-
  exists_file(HdtFile), !,
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph(G, Hdt, HdtFile, NTriplesFile)),
  debug(z(ext), "HDT → open", []).
% N-Triples → HDT
hdt_load(HdtFile, NTriplesFile, G, Opts) :-
  file_name_extension(Base, hdt, HdtFile),
  file_name_extension(Base, 'nt.gz', NTriplesFile),
  exists_file(NTriplesFile), !,
  hdt:hdt_create_from_file(HdtFile, NTriplesFile, Opts),
  debug(z(ext), "N-Triples → HDT", []),
  hdt_load(HdtFile, NTriplesFile, G, Opts).
% N-Quads → N-Triples
hdt_load(HdtFile, NTriplesFile, G, Opts) :-
  file_name_extension(Base, hdt, HdtFile),
  file_name_extension(Base, 'nq.gz', NQuadsFile),
  exists_file(NQuadsFile), !,
  file_name_extension(Base, 'nt.gz', NTriplesFile),
  setup_call_cleanup(
    ensure_ntriples(NQuadsFile, NTriplesFile),
    (
      debug(z(ext), "N-Quads → N-Triples", []),
      hdt_load(HdtFile, NTriplesFile, G, Opts)
    ),
    delete_file(NTriplesFile)
  ).



%! hdt_load_to_memory(+G) is det.

hdt_load_to_memory(G) :-
  hdt_load(G),
  hdt(S, P, O, G),
  rdf_assert(S, P, O, G),
  fail.
hdt_load_to_memory(_).



%! hdt_refresh(+G) is det.

hdt_refresh(G) :-
  hdt_graph(G, _, HdtFile, _), !,
  hdt_unload(G),
  hdt_load(HdtFile, G).



%! hdt_unload(+G) is det.

hdt_unload(G) :-
  hdt_graph(G, Hdt),
  hdt:hdt_close(Hdt),
  retract(hdt_graph(G,Hdt,_,_)).





% HELPERS %

%! ensure_ntriples(+From, +To) is det.

ensure_ntriples(From, To) :-
  setup_call_cleanup(
    gzopen(To, write, Sink),
    with_output_to(Sink,
      rdf_call_on_tuples(From, [_,S,P,O,G]>>gen_ntriple(S, P, O, G))
    ),
    close(Sink)
  ).
