:- module(
  hdt_ext,
  [
    hdt/3,                      % ?S, ?P, ?O
    hdt/4,                      % ?S, ?P, ?O, ?G
    hdt0/4,                     % ?S, ?P, ?O, +Hdt
    hdt_bnode/1,                % ?B
    hdt_bnode/2,                % ?B, ?G
    hdt_bnode0/2,               % ?B, +Hdt
    hdt_call_on_file/2,         % +File, :Goal_1
    hdt_call_on_graph/2,        % ?G, :Goal_1
    hdt_datatype/1,             % ?D
    hdt_datatype/2,             % ?D, ?G
    hdt_datatype0/2,            % ?D, +Hdt
    hdt_iri/1,                  % ?Iri
    hdt_iri/2,                  % ?Iri, ?G
    hdt_iri0/2,                 % ?Iri, +Hdt
    hdt_literal/1,              % ?Lit
    hdt_literal/2,              % ?Lit, ?G
    hdt_literal0/2,             % ?Lit, +Hdt
    hdt_lts/1,                  % ?Lit
    hdt_lts/2,                  % ?Lit, ?G
    hdt_lts0/2,                 % ?Lit, +Hdt
    hdt_meta/4,                 % ?S, ?P, ?O, +G
    hdt_meta0/4,                % ?S, ?P, ?O, +Hdt
    hdt_name/1,                 % ?Name, ?G
    hdt_name/2,                 % ?Name, ?G
    hdt_name0/2,                % ?Name, +Hdt
    hdt_node/1,                 % ?Node, ?G
    hdt_node/2,                 % ?Node, ?G
    hdt_node0/2,                % ?Node, +Hdt
    hdt_number_of_objects/2,    % ?G, -NumOs
    hdt_number_of_properties/2, % ?G, -NumPs
    hdt_number_of_subjects/2,   % ?G, -NumSs
    hdt_number_of_triples/2,    % ?G, -NumTriples
    hdt_object/1,               % ?O, ?G
    hdt_object/2,               % ?O, ?G
    hdt_object0/2,              % ?O, +Hdt
    hdt_predicate/1,            % ?P, ?G
    hdt_predicate/2,            % ?P, ?G
    hdt_predicate0/2,           % ?P, +Hdt
    hdt_prepare/1,              % +File
    hdt_prepare/2,              % +File, -HdtFile
    hdt_remove/1,               % +File
    hdt_subject/1,              % ?S, ?G
    hdt_subject/2,              % ?S, ?G
    hdt_subject0/2,             % ?S, +Hdt
    hdt_term/1,                 % ?Term, ?G
    hdt_term/2,                 % ?Term, ?G
    hdt_term0/2                 % ?Term, +Hdt
  ]
).

/** <module> HDT extensions

@author Wouter Beek
@version 2016/06, 2016/08
*/

:- use_module(library(hdt), []).
:- use_module(library(hdt/hdt_io), []).
:- use_module(library(os/file_ext)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_term)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).


:- meta_predicate
    hdt_call_on_file(+, 1),
    hdt_call_on_graph(?, 1).


:- rdf_meta
   hdt(r, r, o),
   hdt(r, r, o, r),
   hdt0(r, r, o, +),
   hdt_bnode(?, r),
   hdt_call_on_graph(r, :),
   hdt_datatype(r),
   hdt_datatype(r, r),
   hdt_datatype0(r, +),
   hdt_iri(r),
   hdt_iri(r, r),
   hdt_iri0(r, +),
   hdt_literal(o),
   hdt_literal(o, r),
   hdt_literal0(o, +),
   hdt_lts(o),
   hdt_lts(o, r),
   hdt_lts0(o, +),
   hdt_meta(r, r, o, r),
   hdt_meta0(r, r, o, +),
   hdt_name(o),
   hdt_name(o, r),
   hdt_name0(o, +),
   hdt_node(o),
   hdt_node(o, r),
   hdt_node0(o, +),
   hdt_number_of_objects(r, -),
   hdt_number_of_properties(r, -),
   hdt_number_of_subjects(r, -),
   hdt_number_of_triples(r, -),
   hdt_object(o),
   hdt_object(o, r),
   hdt_object0(o, +),
   hdt_predicate(r),
   hdt_predicate(r, r),
   hdt_predicate0(r, +),
   hdt_subject(r),
   hdt_subject(r, r),
   hdt_subject0(r, +),
   hdt_term(o),
   hdt_term(o, r),
   hdt_term0(o, +).





%! hdt(?S, ?P, ?O) is nondet.
%! hdt(?S, ?P, ?O, ?G) is nondet.
%! hdt(?S, ?P, ?O, +Hdt) is nondet.

hdt(S, P, O) :-
  distinct(rdf(S,P,O), hdt(S, P, O, _)).


hdt(S, P, O, G) :-
  hdt_call_on_graph(G, hdt0(S, P, O)).


hdt0(S, P, O, Hdt) :-
  % @hack
  catch(hdt:hdt_search(Hdt, S, P, O), _, fail).



%! hdt_bnode(?B) is nondet.
%! hdt_bnode(?B, ?G) is nondet.
%! hdt_bnode0(?B, +Hdt) is nondet.

hdt_bnode(B) :-
  hdt_bnode(B, _).


hdt_bnode(B, G) :-
  hdt_call_on_graph(G, hdt_bnode0(B)).


hdt_bnode0(B, Hdt) :-
  hdt_name0(B, Hdt),
  q_is_bnode(B).



%! hdt_call_on_file(+File, :Goal_1) is det.

hdt_call_on_file(File, Goal_1) :-
  setup_call_cleanup(
    hdt:hdt_open(Hdt, File),
    call(Goal_1, Hdt),
    hdt:hdt_close(Hdt)
  ).



%! hdt_call_on_graph(?G, :Goal_1) is det.

hdt_call_on_graph(G, Goal_1) :-
  q_store_graph(G),
  q_file_graph(File, hdt, G),
  exists_file(File),
  hdt_call_on_file(File, Goal_1).



%! hdt_datatype(?D) is nondet.
%! hdt_datatype(?D, ?G) is nondet.
%! hdt_datatype0(?D, +Hdt) is nondet.

hdt_datatype(D) :-
  distinct(D, hdt_datatype(D, _)).


hdt_datatype(D, G) :-
  hdt_call_on_graph(G, hdt_datatype0(D)).


hdt_datatype0(D, Hdt) :-
  hdt_literal0(Lit, Hdt),
  q_literal_datatype(Lit, D).



%! hdt_iri(?Iri) is nondet.
%! hdt_iri(?Iri, ?G) is nondet.
%! hdt_iri0(?Iri, +Hdt) is nondet.

hdt_iri(Iri) :-
  distinct(Iri, hdt_iri(Iri, _)).


hdt_iri(Iri, G) :-
  hdt_call_on_graph(G, hdt_iri0(Iri)).


hdt_iri0(Iri, Hdt) :-
  hdt_name0(Iri, Hdt),
  q_is_iri(Iri).



%! hdt_literal(?Lit) is nondet.
%! hdt_literal(?Lit, ?G) is nondet.
%! hdt_literal0(?Lit, +Hdt) is nondet.

hdt_literal(Lit) :-
  distinct(Lit, hdt_literal(Lit, _)).


hdt_literal(Lit, G) :-
  hdt_call_on_graph(G, hdt_literal0(Lit)).


hdt_literal0(Lit, Hdt) :-
  hdt_object0(Lit, Hdt),
  q_is_literal(Lit).



%! hdt_lts(?Lit) is nondet.
%! hdt_lts(?Lit, ?G) is nondet.
%! hdt_lts0(?Lit, +Hdt) is nondet.

hdt_lts(Lit) :-
  distinct(Lit, hdt_lts(Lit, _)).


hdt_lts(Lit, G) :-
  hdt_call_on_graph(G, hdt_lts0(Lit)).


hdt_lts0(Lit, Hdt) :-
  hdt_literal0(Lit, Hdt),
  q_is_lts(Lit).



%! hdt_meta(?S, ?P, ?O, ?G) is nondet.
%! hdt_meta0(?S, ?P, ?O, +Hdt) is nondet.
%
% The following predicates are supported:
%
%   * `'<http://rdfs.org/ns/void#triples>'` with object `N^^xsd:integer`

hdt_meta(S, P, O, G) :-
  hdt_call_on_graph(G, hdt_meta0(S, P, O)).


hdt_meta0(S, P, O, Hdt) :-
  hdt:hdt_header(Hdt, S, P, O).



%! hdt_name(?Name) is nondet.
%! hdt_name(?Name, ?G) is nondet.
%! hdt_name0(?Name, +Hdt) is nondet.

hdt_name(Name) :-
  distinct(Name, hdt_name(Name, _)).


hdt_name(Name, G) :-
  hdt_call_on_graph(G, hdt_name0(Name)).


hdt_name0(Name, Hdt) :-
  hdt:hdt_subject(Hdt, Name).
hdt_name0(Name, Hdt) :-
  hdt:hdt_predicate(Hdt, Name).
hdt_name0(Name, Hdt) :-
  hdt:hdt_object(Hdt, Name).
hdt_name0(Name, Hdt) :-
  hdt:hdt_shared(Hdt, Name).



%! hdt_node(?Node) is nondet.
%! hdt_node(?Node, ?G) is nondet.
%! hdt_node0(?Node, +Hdt) is nondet.

hdt_node(Node) :-
  distinct(Node, hdt_node(Node, _)).


hdt_node(S, G) :-
  hdt_call_on_graph(G, hdt_node0(S)).


hdt_node0(S, Hdt) :-
  hdt_subject0(S, Hdt).
hdt_node0(O, Hdt) :-
  hdt_object0(O, Hdt),
  % Make sure there are no duplicates.
  \+ hdt_subject0(O, Hdt).



%! hdt_number_of_objects(?G, -N) is nondet.

hdt_number_of_objects(G, N) :-
  hdt_meta(_, '<http://rdfs.org/ns/void#distinctObjects>', N^^xsd:integer, G).



%! hdt_number_of_properties(?G, -N) is nondet.

hdt_number_of_properties(G, N) :-
  hdt_meta(_, '<http://rdfs.org/ns/void#properties>', N^^xsd:integer, G).



%! hdt_number_of_subjects(?G, -N) is nondet.

hdt_number_of_subjects(G, N) :-
  hdt_meta(_, '<http://rdfs.org/ns/void#distinctSubjects>', N^^xsd:integer, G).



%! hdt_number_of_triples(?G, -N) is nondet.

hdt_number_of_triples(G, N) :-
  hdt_meta(_, '<http://rdfs.org/ns/void#triples>', N^^xsd:integer, G).



%! hdt_object(?O) is nondet.
%! hdt_object(?O, ?G) is nondet.
%! hdt_object0(?O, +Hdt) is nondet.

hdt_object(O) :-
  distinct(O, hdt_object(O, _)).


hdt_object(O, G) :-
  hdt_call_on_graph(G, hdt_object0(O)).


hdt_object0(O, Hdt) :-
  hdt:hdt_object(Hdt, O).



%! hdt_predicate(?P) is nondet.
%! hdt_predicate(?P, ?G) is nondet.
%! hdt_predicate0(?P, +Hdt) is nondet.

hdt_predicate(P) :-
  distinct(P, hdt_predicate(P, _)).


hdt_predicate(P, G) :-
  hdt_call_on_graph(G, hdt_predicate0(P)).


hdt_predicate0(P, Hdt) :-
  (var(P) -> true ; q_is_predicate(P)),
  hdt:hdt_predicate(Hdt, P).



%! hdt_prepare(+File) is det.
%! hdt_prepare(+File, -HdtFile) is det.

hdt_prepare(File) :-
  hdt_prepare(File, _).


hdt_prepare(File1, File2) :-
  atomic_list_concat([Base|_], ., File1),
  hdt_prepare_base(Base, File2).


hdt_prepare_base(Base, File) :-
  atomic_list_concat([Base,hdt], ., File),
  exists_file(File), !.
hdt_prepare_base(Base, File2) :-
  atomic_list_concat([Base,nt,gz], ., File1),
  exists_file(File1), !,
  atomic_list_concat([Base,hdt], ., File2),
  hdt:hdt_create_from_file(File2, File1, []).
hdt_prepare_base(Base, File3) :-
  atomic_list_concat([Base,nq,gz], ., File1),
  exists_file(File1), !,
  atomic_list_concat([Base,nt,gz], ., File2),
  setup_call_cleanup(
    rdf_change_format(File1, File2, [from_format(nquads),to_format(ntriples)]),
    hdt_prepare_base(Base, File3),
    delete_file(File2)
  ).



%! hdt_remove(+File) is det.

hdt_remove(File) :-
  atomic_list_concat([Base,hdt], ., File),
  atomic_list_concat([Base,index,hdt], ., IndexFile),
  delete_file_msg(IndexFile),
  delete_file_msg(File).



%! hdt_subject(?S) is nondet.
%! hdt_subject(?S, ?G) is nondet.
%! hdt_subject0(?S, +Hdt) is nondet.

hdt_subject(S) :-
  distinct(S, hdt_subject(S, _)).


hdt_subject(S, G) :-
  hdt_call_on_graph(G, hdt_subject0(S)).


hdt_subject0(S, Hdt) :-
  (var(S) -> true ; q_is_subject(S)),
  hdt:hdt_subject(Hdt, S).



%! hdt_term(?Term) is nondet.
%! hdt_term(?Term, ?G) is nondet.
%! hdt_term0(?Term, +Hdt) is nondet.

hdt_term(Term) :-
  distinct(Term, hdt_term(Term, _)).


hdt_term(Name, G) :-
  hdt_call_on_graph(G, hdt_term0(Name)).


hdt_term0(Name, Hdt) :-
  hdt_name0(Name, Hdt).
hdt_term0(B, Hdt) :-
  hdt_bnode0(B, Hdt).
