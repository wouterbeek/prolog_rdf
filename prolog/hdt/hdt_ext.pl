:- module(
  hdt_ext,
  [
    hdt/4,              % +File, ?S, ?P, ?O
    hdt0/4,             % +Hdt,  ?S, ?P, ?O
    hdt_goal/2,         % +File,             :Goal_1
    hdt_header/4,       % +File, ?S, ?P, ?O
    hdt_header0/4,      % +Hdt,  ?S, ?P, ?O
    hdt_last/3,         % +File, ?X, ?L
    hdt_last0/3,        % +Hdt,  ?X, ?L
    hdt_member/3,       % +File, ?X, ?L
    hdt_member0/3,      % +Hdt,  ?X, ?L
    hdt_prepare/1,      % +Base
    hdt_prepare/2,      % +Base,                      +Opts
    hdt_print/4,        % +File, ?S, ?P, ?O
    hdt_print/5,        % +File, ?S, ?P, ?O,          +Opts
    hdt_print0/4,       % +Hdt,  ?S, ?P, ?O
    hdt_print0/5,       % +Hdt,  ?S, ?P, ?O,          +Opts
    hdt_remove/1        % +File
  ]
).

/** <module> HDT extensions

@author Wouter Beek
@version 2016/04-2016/05
*/

:- use_module(library(gen/gen_ntuples)).
:- use_module(library(hdt), []).
:- use_module(library(html/html_ext)).
:- use_module(library(pagination)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- meta_predicate
   hdt_goal(+, 1).

:- rdf_meta
   hdt( +, r, r, o),
   hdt0(+, r, r, o),
   hdt_header( +, r, r, o),
   hdt_header0(+, r, r, o),
   hdt_last( +, r, r),
   hdt_last0(+, r, r),
   hdt_member( +, r, r),
   hdt_member0(+, r, r),
   hdt_print( +, r, r, o),
   hdt_print0(+, r, r, o),
   hdt_print( +, r, r, o, +),
   hdt_print0(+, r, r, o, +).





%! hdt(+File, ?S, ?P, ?O) is nondet.
%! hdt0(+Hdt, ?S, ?P, ?O) is nondet.

hdt(File, S, P, O) :-
  hdt_goal(File, {S,P,O}/[Hdt]>>hdt0(Hdt, S, P, O)).


hdt0(Hdt, S, P, O) :-
  hdt:hdt_search(Hdt, S, P, O).



%! hdt_goal(+File, :Goal_1) is det.

hdt_goal(File, Goal_1) :-
  hdt_prepare(File),
  setup_call_cleanup(
    hdt:hdt_open(Hdt, File),
    call(Goal_1, Hdt),
    hdt:hdt_close(Hdt)
  ).



%! hdt_header(+File, ?S, ?P, ?O) is nondet.
%! hdt_header0(+Hdt, ?S, ?P, ?O) is nondet.
% The following predicates are supported:
%   - `<http://rdfs.org/ns/void#triples>` with object `N^^xsd:integer`

hdt_header(File, S, P, O) :-
  hdt_goal(File, {S,P,O}/[Hdt]>>hdt_header0(Hdt, S, P, O)).


hdt_header0(Hdt, S, P, O) :-
  hdt:hdt_header(Hdt, S, P, O).



%! hdt_last(+File, ?L, ?X) is nondet.
%! hdt_last0(+Hdt, ?L, ?X) is nondet.

hdt_last(File, L, X) :-
  hdt_goal(File, {L,X}/[Hdt]>>hdt_last0(Hdt, L, X)).


hdt_last0(Hdt, L, X) :-
  rdf_is_subject(L), !,
  hdt0(Hdt, L, rdf:rest, T),
  (   rdf_equal(T, rdf:nil)
  ->  hdt0(Hdt, L, rdf:first, X)
  ;   hdt_last0(Hdt, T, X)
  ).



%! hdt_member(+File, ?X, ?L) is nondet.
%! hdt_member0(+Hdt, ?X, ?L) is nondet.

hdt_member(File, X, L) :-
  hdt_goal(File, {X,L}/[Hdt]>>hdt_member0(Hdt, X, L)).


hdt_member0(Hdt, X, L) :-
  ground(X), !,
  (   hdt_member2(Hdt, X, L)
  ->  true
  ).
hdt_member0(Hdt, X, L) :-
  hdt_member2(Hdt, X, L).


hdt_member2(Hdt, X, L) :-
  hdt0(Hdt, L, rdf:first, X).
hdt_member2(Hdt, X, L) :-
  hdt0(Hdt, L, rdf:rest, L0),
  hdt_member2(Hdt, X, L0).



%! hdt_prepare(+File) is det.
%! hdt_prepare(+File, +Opts) is det.
% Options are passed to hdt_create_from_file/3.

hdt_prepare(File) :-
  hdt_prepare(File, []).


hdt_prepare(HdtFile, _) :-
  exists_file(HdtFile), !.
hdt_prepare(HdtFile, Opts) :-
  file_name_extension(Base, hdt, HdtFile),
  file_name_extension(Base, 'nt.gz', NTriplesFile),
  exists_file(NTriplesFile), !,
  file_name_extension(Base, hdt, HdtFile),
  hdt:hdt_create_from_file(HdtFile, NTriplesFile, Opts).
hdt_prepare(HdtFile, Opts) :-
  file_name_extension(Base, hdt, HdtFile),
  file_name_extension(Base, 'nq.gz', NQuadsFile),
  exists_file(NQuadsFile), !,
  file_name_extension(Base, 'nt.gz', NTriplesFile),
  setup_call_cleanup(
    ensure_ntriples(NQuadsFile, NTriplesFile),
    hdt_prepare(HdtFile, Opts),
    delete_file(NTriplesFile)
  ).



%! hdt_print(+File, ?S, ?P, ?O) is nondet.
%! hdt_print(+File, ?S, ?P, ?O, +Opts) is nondet.
%! hdt_print0(+Hdt, ?S, ?P, ?O) is nondet.
%! hdt_print0(+Hdt, ?S, ?P, ?O, +Opts) is nondet.

hdt_print(File, S, P, O) :-
  hdt_goal(File, {S,P,O}/[Hdt]>>hdt_print0(Hdt, S, P, O, _{})).


hdt_print(File, S, P, O, Opts) :-
  hdt_goal(File, {S,P,O,Opts}/[Hdt]>>hdt_print0(Hdt, S, P, O, Opts)).


hdt_print0(Hdt, S, P, O) :-
  hdt_print0(Hdt, S, P, O, _{}).


hdt_print0(Hdt, S, P, O, Opts) :-
  hdt_pagination0(
    Hdt, S, P, O,
    {Opts}/[Results]>>rdf_print_triples(Results, Opts),
    Opts
  ).



hdt_remove(File) :-
  (exists_file(File) -> delete_file(File) ; true),
  atomic_list_concat([File,index], ., IndexFile),
  (exists_file(IndexFile) -> delete_file(IndexFile) ; true).





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
