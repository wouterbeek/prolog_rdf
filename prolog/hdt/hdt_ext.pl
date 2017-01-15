:- module(
  hdt_ext,
  [
    hdt0/4,                     % ?S, ?P, ?O, +Hdt
    hdt_bnode0/2,               % ?B, +Hdt
    hdt_call_on_file/2,         % +File, :Goal_1
    hdt_datatype0/2,            % ?D, +Hdt
    hdt_estimate_complexity0/5, % ?S, ?P, ?O, -Est, +Hdt
    hdt_header0/4,              % ?S, ?P, ?O, +Hdt
    hdt_iri0/2,                 % ?Iri, +Hdt
    hdt_literal0/2,             % ?Lit, +Hdt
    hdt_lts0/2,                 % ?Lit, +Hdt
    hdt_meta0/4,                % ?S, ?P, ?O, +Hdt
    hdt_name0/2,                % ?Name, +Hdt
    hdt_node0/2,                % ?Node, +Hdt
    hdt_object0/2,              % ?O, +Hdt
    hdt_predicate0/2,           % ?P, +Hdt
    hdt_prepare_file/1,         % +File
    hdt_prepare_file/2,         % +File, -HdtFile
    hdt_remove/1,               % +File
    hdt_remove_index/1,         % +File
    hdt_subject0/2,             % ?S, +Hdt
    hdt_term0/2                 % ?Term, +Hdt
  ]
).

/** <module> HDT extensions

@author Wouter Beek
@version 2016/06-2017/01
*/

:- use_module(library(hdt), []).
:- use_module(library(os/file_ext)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    hdt_call_on_file(+, 1).

:- rdf_meta
   hdt0(r, r, o, +),
   hdt_datatype0(r, +),
   hdt_estimate_complexity0(r, r, o, -, +),
   hdt_iri0(r, +),
   hdt_literal0(o, +),
   hdt_lts0(o, +),
   hdt_meta0(r, r, o, +),
   hdt_name0(o, +),
   hdt_node0(o, +),
   hdt_object0(o, +),
   hdt_predicate0(r, +),
   hdt_subject0(r, +),
   hdt_term0(o, +).





%! hdt0(?S, ?P, ?O, +Hdt) is nondet.

hdt0(S, P, O, Hdt) :-
  hdt:hdt_search(Hdt, S, P, O).



%! hdt_bnode0(?B, +Hdt) is nondet.

hdt_bnode0(B, Hdt) :-
  hdt_name0(B, Hdt),
  rdf_is_bnode(B).



%! hdt_call_on_file(+File, :Goal_1) is det.

hdt_call_on_file(File, Goal_1) :-
  setup_call_cleanup(
    hdt:hdt_open(Hdt, File),
    call(Goal_1, Hdt),
    hdt:hdt_close(Hdt)
  ).



%! hdt_datatype0(?D, +Hdt) is nondet.

hdt_datatype0(D, Hdt) :-
  hdt_literal0(Lit, Hdt),
  q_literal_datatype(Lit, D).



%! hdt_estimate_complexity0(?S, ?P, ?O, -Est, +Hdt) is det.

hdt_estimate_complexity0(S, P, O, Est, Hdt) :-
  hdt:hdt_search_cost(Hdt, S, P, O, Est0),
  Est is integer(Est0).



%! hdt_header0(?S, ?P, ?O, +Hdt) is nondet.

hdt_header0(S, P, O, Hdt) :-
  hdt:hdt_header(Hdt, S, P, O).



%! hdt_iri0(?Iri, +Hdt) is nondet.

hdt_iri0(Iri, Hdt) :-
  hdt_name0(Iri, Hdt),
  q_is_iri(Iri).



%! hdt_literal0(?Lit, +Hdt) is nondet.

hdt_literal0(Lit, Hdt) :-
  hdt_object0(Lit, Hdt),
  q_is_literal(Lit).



%! hdt_lts0(?Lit, +Hdt) is nondet.

hdt_lts0(Lit, Hdt) :-
  hdt_literal0(Lit, Hdt),
  q_is_lts(Lit).



%! hdt_meta0(?S, ?P, ?O, +Hdt) is nondet.
%
% The following predicates are supported:
%
%   * `'<http://rdfs.org/ns/void#triples>'` with object `N^^xsd:integer`

hdt_meta0(S, P, O, Hdt) :-
  hdt:hdt_header(Hdt, S, P, O).



%! hdt_name0(?Name, +Hdt) is nondet.

hdt_name0(Name, Hdt) :-
  (ground(Name) -> q_is_subject(Name) ; true),
  hdt:hdt_subject(Hdt, Name).
hdt_name0(Name, Hdt) :-
  (ground(Name) -> q_is_predicate(Name) ; true),
  hdt:hdt_predicate(Hdt, Name).
hdt_name0(Name, Hdt) :-
  hdt:hdt_object(Hdt, Name).
hdt_name0(Name, Hdt) :-
  hdt:hdt_shared(Hdt, Name).



%! hdt_node0(?Node, +Hdt) is nondet.

hdt_node0(S, Hdt) :-
  hdt_subject0(S, Hdt).
hdt_node0(O, Hdt) :-
  hdt_object0(O, Hdt),
  % Make sure there are no duplicates.
  \+ hdt_subject0(O, Hdt).



%! hdt_object0(?O, +Hdt) is nondet.

hdt_object0(O, Hdt) :-
  hdt:hdt_object(Hdt, O).
hdt_object0(O, Hdt) :-
  hdt:hdt_shared(Hdt, O).



%! hdt_predicate0(?P, +Hdt) is nondet.

hdt_predicate0(P, Hdt) :-
  (var(P) -> true ; q_is_predicate(P)),
  hdt:hdt_predicate(Hdt, P).



%! hdt_prepare_file(+File) is det.
%! hdt_prepare_file(+File, -HdtFile) is det.

hdt_prepare_file(File) :-
  hdt_prepare_file(File, _).


hdt_prepare_file(File1, File2) :-
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
    rdf_reserialize(
      File1,
      File2,
      [rdf_media_type(application/'n-quads')],
      [rdf_media_type(application/'n-triples')]
    ),
    hdt_prepare_base(Base, File3),
    delete_file(File2)
  ).



%! hdt_remove(+File) is det.

hdt_remove(File) :-
  delete_file_msg(File),
  hdt_remove_index(File).



%! hdt_remove_index(+File) is det.
%
% Remove the HDT index file for the given HDT file.

hdt_remove_index(File) :-
  file_name_extension(Base, hdt, File),
  file_name_extension(Base, 'hdt.index', IndexFile),
  delete_file_msg(IndexFile).



%! hdt_subject0(?S, +Hdt) is nondet.

hdt_subject0(S, Hdt) :-
  (var(S) -> true ; q_is_subject(S)),
  (   hdt:hdt_subject(Hdt, S)
  ;   hdt:hdt_shared(Hdt, S)
  ).



%! hdt_term0(?Term, +Hdt) is nondet.

hdt_term0(Name, Hdt) :-
  hdt_name0(Name, Hdt).
hdt_term0(BNode, Hdt) :-
  hdt_bnode0(BNode, Hdt).
