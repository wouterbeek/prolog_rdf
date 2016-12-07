:- module(
     hdt2vec,
     [
       export/0,
       export/2,  % +Depth:nonneg, +NumPaths:nonneg
       hdt2rdf/2, % +HdtFile, +NtFile
       merge/1    % +NtFile
     ]
   ).

/** <module> HDT-2-Vec

@author Wouter Beek
@version 2016/12/07
*/

:- use_module(library(aggregate)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(list_ext)).
:- use_module(library(os/io)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_term)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/turtle), []).
:- use_module(library(yall)).

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
  q_literal_lex(V^^D, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write('^^'),
  write_iri(D).
write_literal(V@LTag) :-
  q_literal_lex(V@LTag, Lex),
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
    ll_call_on_files(+, 3).

%! ll_call_on_files(+MT, +N, :Goal_3) is det.

ll_call_on_files(MT, N, Goal_3a) :-
  forall(
    between(1, N, _),
    (
      copy_term(Goal_3a, Goal_3b),
      q_file(MT, NtFile),
      call_on_stream(NtFile, Goal_3b)
    )
  ).



%! hdt2rdf(+HdtFile:atom, +NtFile:atom) is det.

hdt2rdf(HdtFile, NtFile) :-
  call_to_ntriples(NtFile, hdt2rdf(HdtFile)).

hdt2rdf(HdtFile, State, Out) :-
  hdt_call_on_file(HdtFile, hdt2rdf0(State, Out)).

hdt2rdf0(State, Out, Hdt) :-
  forall(hdt0(S, P, O, Hdt), gen_ntuple(S, P, O, State, Out)).



merge(NtFile) :-
  call_to_stream(NtFile, merge0).

merge0(Out) :-
  ll_call_on_files(
    application/'n-triples',
    100,
    {Out}/[In,Path,Path]>>copy_data_stream(In, Out)
  ).
