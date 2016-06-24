:- module(
  hdt_ext,
  [
    hdt/4,           % ?S, ?P, ?O, +File
    hdt0/4,          % ?S, ?P, ?O, +Hdt
    hdt_bnode/2,     % ?B, ?G
    hdt_datatype/1,  % ?D
    hdt_datatype/2,  % ?D, ?G
    hdt_goal/2,      % +File, :Goal_1
    hdt_header/4,    % ?S, ?P, ?O, +File
    hdt_header0/4,   % ?S, ?P, ?O, +Hdt
    hdt_iri/2,       % ?Iri, ?G
    hdt_last/3,      % ?X, ?L, +File
    hdt_last0/3,     % ?X, ?L, +Hdt
    hdt_literal/2,   % ?Lit, ?G
    hdt_lts/1,       % ?Lit
    hdt_lts/2,       % ?Lit, ?G
    hdt_member/3,    % ?X, ?L, +File
    hdt_member0/3,   % ?X, ?L, +Hdt
    hdt_name/2,      % ?Name, ?G
    hdt_node/2,      % ?Node, ?G
    hdt_object/2,    % ?O, ?G
    hdt_predicate/2, % ?P, ?G
    hdt_prepare/1,   % +File
    hdt_prepare/2,   % +File, +Opts
    hdt_print/4,     % ?S, ?P, ?O, +File
    hdt_print0/4,    % ?S, ?P, ?O, +Hdt
    hdt_print/5,     % ?S, ?P, ?O, +File, +Opts
    hdt_print0/5,    % ?S, ?P, ?O, +Hdt, +Opts
    hdt_remove/1,    % +File
    hdt_subject/2,   % ?S, ?G
    hdt_term/2,      % ?Term, ?G
    hdt_tree/3,      % ?S, -Tree, +File
    hdt_tree0/3      % ?S, -Tree, +Hdt
  ]
).

/** <module> HDT extensions

@author Wouter Beek
@version 2016/04-2016/06
*/

:- use_module(library(gen/gen_ntuples)).
:- use_module(library(hdt), []).
:- use_module(library(html/html_ext)).
:- use_module(library(pagination)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- meta_predicate
   hdt_goal(+, 1).

:- rdf_meta
   hdt( r, r, o, +),
   hdt0(r, r, o, +),
   hdt_bnode(?, r),
   hdt_datatype(r),
   hdt_datatype(r, r),
   hdt_header( r, r, o, +),
   hdt_header0(r, r, o, +),
   hdt_iri(r, r),
   hdt_last( r, r, +),
   hdt_last0(r, r, +),
   hdt_literal(o, r),
   hdt_literal(o, r, ?, ?),
   hdt_lts(o),
   hdt_lts(o, r),
   hdt_member( r, r, +),
   hdt_member0(r, r, +),
   hdt_print( r, r, o, +),
   hdt_print0(r, r, o, +),
   hdt_print( r, r, o, +, +),
   hdt_print0(r, r, o, +, +),
   hdt_tree(r, -, +),
   hdt_tree0(r, -, +).





%! hdt(?S, ?P, ?O, +File) is nondet.
%! hdt0(?S, ?P, ?O, +Hdt) is nondet.

hdt(S, P, O, File) :-
  hdt_goal(File, hdt0(S, P, O)).


hdt0(S, P, O, Hdt) :-
  hdt:hdt_search(Hdt, S, P, O).



%! hdt_bnode(?B, ?G) is nondet.

hdt_bnode(B, G) :-
  z_bnode0(disk, B, G).



%! hdt_datatype(?D) is nondet.
%! hdt_datatype(?D, ?G) is nondet.

hdt_datatype(D) :-
  z_datatype(disk, D).


hdt_datatype(D, G) :-
  z_datatype(disk, D, G).



%! hdt_goal(+File, :Goal_1) is det.

hdt_goal(File, Goal_1) :-
  hdt_prepare(File),
  setup_call_cleanup(
    hdt:hdt_open(Hdt, File),
    call(Goal_1, Hdt),
    hdt:hdt_close(Hdt)
  ).



%! hdt_header( ?S, ?P, ?O, +File) is nondet.
%! hdt_header0(?S, ?P, ?O, +Hdt ) is nondet.
% The following predicates are supported:
%   - `<http://rdfs.org/ns/void#triples>` with object `N^^xsd:integer`

hdt_header(S, P, O, File) :-
  hdt_goal(File, hdt_header0(S, P, O)).


hdt_header0(S, P, O, Hdt) :-
  hdt:hdt_header(Hdt, S, P, O).



%! hdt_iri(?Iri, ?G) is nondet.

hdt_iri(Iri, G) :-
  z_iri(disk, Iri, G).



%! hdt_last( ?L, ?X, +File) is nondet.
%! hdt_last0(?L, ?X, +Hdt ) is nondet.

hdt_last(L, X, File) :-
  hdt_goal(File, hdt_last0(L, X)).


hdt_last0(L, X, Hdt) :-
  rdf_is_subject(L), !,
  hdt0(L, rdf:rest, T, Hdt),
  (   rdf_equal(T, rdf:nil)
  ->  hdt0(L, rdf:first, X, Hdt)
  ;   hdt_last0(T, X, Hdt)
  ).



%! hdt_lts(?Lit) is nondet.
%! hdt_lts(?Lit, ?G) is nondet.

hdt_lts(Lit) :-
  z_lts(disk, Lit).


hdt_lts(Lit, G) :-
  z_lts(disk, Lit, G).



%! hdt_literal(?Lit, ?G) is nondet.

hdt_literal(Lit, G) :-
  z_literal(disk, Lit, G).



%! hdt_member( ?X, ?L, +File) is nondet.
%! hdt_member0(?X, ?L, +Hdt ) is nondet.

hdt_member(X, L, File) :-
  hdt_goal(File, hdt_member0(X, L)).


hdt_member0(X, L, Hdt) :-
  ground(X), !,
  (   hdt_member2(X, L, Hdt)
  ->  true
  ).
hdt_member0(X, L, Hdt) :-
  hdt_member2(X, L, Hdt).


hdt_member2(X, L, Hdt) :-
  hdt0(L, rdf:first, X, Hdt).
hdt_member2(X, L, Hdt) :-
  hdt0(L, rdf:rest, L0, Hdt),
  hdt_member2(X, L0, Hdt).



%! hdt_name(?Name, ?G) is nondet.

hdt_name(Name, G) :-
  z_name(disk, Name, G).



%! hdt_node(?Node, ?G) is nondet.

hdt_node(Node, G) :-
  z_node(disk, Node, G).



%! hdt_object(?O, ?G) is nondet.

hdt_object(O, G) :-
  z_object(disk, O, G).



%! hdt_predicate(?P, ?G) is nondet.

hdt_predicate(P, G) :-
  z_predicate(disk, P, G).



%! hdt_prepare(+File       ) is det.
%! hdt_prepare(+File, +Opts) is det.
%
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



%! hdt_print(?S, ?P, ?O, +File) is nondet.
%! hdt_print0(?S, ?P, ?O, +Hdt) is nondet.
%! hdt_print(?S, ?P, ?O, +File, +Opts) is nondet.
%! hdt_print0(?S, ?P, ?O, +Hdt, +Opts) is nondet.

hdt_print(S, P, O, File) :-
  hdt_print(S, P, O, File, _{}).


hdt_print(S, P, O, File, Opts) :-
  hdt_goal(File, {S,P,O,Opts}/[Hdt]>>hdt_print0(S, P, O, Hdt, Opts)).


hdt_print0(S, P, O, Hdt) :-
  hdt_print0(S, P, O, Hdt, _{}).


hdt_print0(S, P, O, Hdt, Opts) :-
  pagination(rdf(S,P,O), hdt0(S, P, O, Hdt), Opts, Result),
  pagination_result(Result,
    {Opts}/[Results]>>rdf_print_triples(Results, Opts)
  ).



%! hdt_remove(+File) is det.

hdt_remove(File) :-
  (exists_file(File) -> delete_file(File) ; true),
  atomic_list_concat([File,index], ., IndexFile),
  (exists_file(IndexFile) -> delete_file(IndexFile) ; true).



%! hdt_subject(?S, ?G) is nondet.

hdt_subject(S, G) :-
  z_subject(disk, S, G).



%! hdt_term(?Term, ?G) is nondet.

hdt_term(P, G) :-
  z_term(disk, P, G).



%! hdt_tree(?S, -Tree, +File) is det.
%! hdt_tree0(?S, -Tree, +Hdt) is det.

hdt_tree(S, Tree, File) :-
  hdt_goal(File, {S,Tree}/[Hdt]>>hdt_tree0(S, Tree, Hdt)).


hdt_tree0(S, Tree, Hdt) :-
  distinct(S, hdt0(S, P, O, Hdt)),
  hdt_tree0([S], Hdt, [], [], Tree).


hdt_tree0([], _, _, Tree, Tree) :- !.
hdt_tree0([H|T], G, Hist, Tree, Sol) :-
  memberchk(H, Hist), !,
rdf_tree0([S|T1], G, Hist1, Tree1, Sol) :-
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O, G), Triples),
  aggregate_all(set(O), (rdf(S, _, O, G), \+ rdf_is_literal(O)), Os),
  ord_union(T1, Os, T2),
  ord_add_element(Hist1, S, Hist2),
  ord_union(Tree1, Triples, Tree2),
  rdf_tree0(T2, G, Hist2, Tree2, Sol).





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
