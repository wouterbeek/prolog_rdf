:- module(
     hdt2vec,
     [
       export/0,
       export/2,  % +Depth:nonneg, +NumPaths:nonneg
       hdt2rdf/0,
       merge/1    % +NtFile
     ]
   ).

/** <module> HDT-2-Vec

@author Wouter Beek
@version 2016/12
*/

:- use_module(library(aggregate)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(list_ext)).
:- use_module(library(os/io)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_term)).
:- use_module(library(random)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle), []).
:- use_module(library(settings)).
:- use_module(library(yall)).

:- set_setting(q_io:store_dir, '/scratch/wbeek/crawls/13/store/').

export :-
  export(1, 1).

export(Depth, NumPaths) :-
  hdt_prepare_file('bgt-valkenswaard.nt.gz', HdtFile),
  hdt_call_on_file(HdtFile, export_document0(Depth, NumPaths)).

export_document0(Depth, NumPaths, Hdt) :-
  forall(hdt_subject0(S, Hdt), export_subject0(Hdt, S, Depth, NumPaths)).

export_subject0(Hdt, S, Depth, NumPaths) :-
  forall(between(1, NumPaths, _), export_path0(Hdt, S, Depth)).

export_path0(Hdt, S, Depth) :-
  export_path0(Hdt, S, Depth, Path),
  write_path1(Path).

export_path0(_, O, 0, [O]) :- !.
export_path0(Hdt, S, Depth1, [S,P|T]) :-
  rnd_hdt0(S, _, _, rdf(S,P,O), Hdt),
  Depth2 is Depth1 - 1,
  export_path0(Hdt, O, Depth2, T).

write_path1([S|T]) :-
  write_subject(S),
  write_sep,
  write_path2(T).

write_path2([]) :- !.
write_path2([Node]) :- !,
  write_sep,
  write_object(Node).
write_path2([P,Node|T]) :-
  write_predicate(P),
  write_sep,
  write_object(Node),
  write_sep,
  write_path2(T).

write_bnode(BNode) :-
  write(BNode).

write_iri(Iri) :-
  turtle:turtle_write_uri(current_output, Iri).

write_literal(V^^D) :- !,
  rdf_literal_lexical_form(V^^D, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  write_iri(D).
write_literal(V@LTag) :-
  rdf_literal_lexical_form(V@LTag, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  put_char('@'),
  write(LTag).

write_object(S) :-
  write_subject(S), !.
write_object(Lit) :-
  write_literal(Lit).

write_predicate(Iri) :-
  write_iri(Iri).

write_subject(BNode) :-
  rdf_is_bnode(BNode), !,
  write_bnode(BNode).
write_subject(Iri) :-
  rdf_is_iri(Iri), !,
  write_predicate(Iri).

write_sep :-
  write(",").



%! rnd_hdt0(?S, ?P, ?O, -Triple, +Hdt) is det.

rnd_hdt0(S, P, O, Triple, Hdt) :-
  aggregate_all(set(rdf(S,P,O)), hdt0(S, P, O, Hdt), Triples),
  length(Triples, NumTriples),
  random_between(1, NumTriples, Index),
  nth1chk(Index, Triples, Triple).



:- meta_predicate
    ll_call_on_files(+, +, 1).

%! ll_call_on_files(+MT, +N, :Goal_1) is det.

ll_call_on_files(MT, N, Goal_1a) :-
  forall(
    between(1, N, _),
    (
      copy_term(Goal_1a, Goal_1b),
      q_file(MT, File),
      call(Goal_1b, File)
    )
  ).



%! hdt2rdf is det.

hdt2rdf :-
  ll_call_on_files(hdt, inf, hdt2rdf).


%! hdt2rdf(+HdtFile:atom) is det.

hdt2rdf(HdtFile) :-
  atomic_list_concat([Base,hdt], ., HdtFile),
  atomic_list_concat([Base,nt,gz], ., NtFile),
  hdt2rdf(HdtFile, NtFile).


%! hdt2rdf(+HdtFile:atom, +NtFile:atom) is det.

hdt2rdf(HdtFile, NtFile) :-
  rdf_call_to_ntriples(NtFile, hdt2rdf(HdtFile)).

hdt2rdf(HdtFile, State, Out) :-
  hdt_call_on_file(HdtFile, hdt2rdf0(State, Out)).

hdt2rdf0(State, Out, Hdt) :-
  forall(hdt0(S, P, O, Hdt), rdf_write_ntuple(S, P, O, State, Out)).



merge(NtFile) :-
  call_to_stream(NtFile, merge0).

merge0(Out) :-
  ll_call_on_files(ntriples, 2, merge_file0(Out)).

merge_file0(Out, NtFile) :-
  call_on_stream(NtFile, {Out}/[In,Path,Path]>>copy_stream_data(In, Out)).
