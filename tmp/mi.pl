:- module(
  mi,
  [
    mi/7 % +Doc, +Trans, +Mean,
         % -NumSubjects, -Cx, -Cy, -Cxy
  ]
).

/** <module> Mutual Information (MI)

@author Wouter Beek
@author Frank van Harmelen
@version 2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg)).
:- use_module(library(os/io)).
:- use_module(library(random)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(service/ll_api)).
:- use_module(library(uri)).
:- use_module(library(yall)).

:- thread_local
   name_alias/2.





%! mi(
%!   +Doc, +Trans, +Mean,
%!   -NumSubjects, -Cx, -Cy, -Cxy
%! ) is nondet.
% @arg Trans Either `none`, `no_prefix` or `rnd_iri`.
% @arg Mean Either `predset` or `typeset`.

mi(Doc, Trans, Mean, NumSubjects, Cx, Cy, Cxy) :-
  doc_download(Doc, Iri),
  doc_name(Doc, Name),
  q_snap((
    rdf_call_on_graph(
      Iri,
      {Name,Trans,Mean,NumSubjects,Cx,Cy,Cxy}/[_,Meta,Meta]>>mi_graph(
        Name, Trans, Mean, NumSubjects, Cx, Cy, Cxy
      )
    )
  )),
  retractall(name_alias(_,_)).


mi_graph(Name, Trans, Mean, NumSubjects, Cx, Cy, Cxy) :-
  aggregate_all(set(S), (rdf_subject(S), rdf_is_iri(S)), Terms),
  length(Terms, NumSubjects),
  maplist(meaning(Mean), Terms, Means),
  name_base0(Name, Base),
  entropy1(Base, Trans, Mean, Terms, Means, Cx, Cy),
  entropy2(Base, Trans, Mean, Terms, Means, Cxy).


entropy1(Base, Trans, Mean, Terms, Means, H1A, H1B) :-
  atomic_list_concat([Base,Trans,Mean,h1a,gz], ., FileA),
  atomic_list_concat([Base,Trans,Mean,h1b,gz], ., FileB),
  call_to_stream(FileA, entropy_terms_writer0(Trans, Terms)),
  size_file(FileA, H1A),
  call_to_stream(FileB, entropy_meanings_writer0(Trans, Means)),
  size_file(FileB, H1B).


entropy2(Base, Trans, Mean, Terms, Means, H1) :-
  atomic_list_concat([Base,Mean,Trans,h2,gz], ., File),
  call_to_stream(File, entropy_pairs_writer(Trans, Terms, Means)),
  size_file(File, H1).





% MEANINGS %

meaning(predset, S, Ps) :- !,
  q_aggregate_all(set(P), rdf(S, P, _), Ps).
meaning(typeset, I, Cs) :-
  q_aggregate_all(set(C), rdfs_instance0(I, C), Cs).





% WRITERS %

entropy_meanings_writer0(Trans, Means, Out) :-
  with_output_to(Out, maplist(entropy_meaning_writer0(Trans), Means)).


entropy_meaning_writer(_, []) :- !,
  nl.
entropy_meaning_writer(Trans, [H]) :- !,
  write_transformed_term(Trans, H),
  nl.
entropy_meaning_writer(Trans, [H|T]) :-
  write_transformed_term(Trans, H),
  write(' '),
  entropy_meaning_writer(Trans, T).



entropy_pairs_writer0(Trans, Terms, Means, Out) :-
  with_output_to(Out, maplist(entropy_pair_writer(Trans), Terms, Means)).


entropy_pair_writer(Trans, Term, Mean) :-
  write_transformed_term(Trans, Term),
  put_char(' '),
  entropy_meaning_writer(Trans, Mean).



entropy_terms_writer(Trans, Terms) :-
  maplist(entropy_term_writer(Trans), Terms).


entropy_term_writer(Trans, Term) :-
  write_transformed_term(Trans, Term),
  nl.





% TRANSFORMATIONS %

term_transformation(no_prefix, Term1, Term2) :-
  rdf_is_iri(Term1), !,
  uri_components(Term1, uri_components(_,_,Path,Query,Frag)),
  uri_components(Term2, uri_components('','',Path,Query,Frag)).
term_transformation(rnd_iri, Term1, Term2) :-
  rdf_is_iri(Term1), !,
  (   name_alias(Term1, Term2)
  ->  true
  ;   dcg_with_output_to(atom(Path), rnd_path),
      uri_components(Term2, uri_components('','',Path,_,_)),
      assert(name_alias(Term1, Term2))
  ).
term_transformation(_, Term, Term).



write_transformed_term(Out, Trans, Term) :-
  term_transformation(Trans, Term, TransTerm),
  write(Out, TransTerm).





% HELPERS %

name_base0(Name, Base) :-
  sub_atom(Name, 0, 2, _, Prefix),
  atom_concat(Prefix, File, Name),
  atomic_list_concat(['',ssd,lodlab,wouter,mi,Prefix], /, Dir),
  with_mutex(make_directory, make_directory_path(Dir)),
  directory_file_path(Dir, File, Base).



rnd_char -->
  {random_between(97, 122, X)},
  [X].



rnd_path -->
  "/",
  #(15, rnd_char).
