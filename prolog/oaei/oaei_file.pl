:- module(
  oaei_file,
  [
    oaei_convert_rdf_to_tsv/2, % +FromFile:atom
                               % +ToFile:atom
    oaei_convert_tsv_to_rdf/2, % +FromFile:atom
                               % +ToFile:atom
    oaei_load_rdf/2, % +File:atom
                     % -Alignments:ordset(pair(iri))
    oaei_load_tsv/2, % +File:atom
                     % -Alignments:ordset(pair(iri))
    oaei_save_rdf/2, % +In
                     % +Alignments:list(pair))
    oaei_save_tsv/2 % +In
                    % +Alignments:list(pair))
  ]
).

/** <module> Ontology Alignment Evaluation Initiative (OAEI)

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(csv_ext)).
:- use_module(library(iostream)).
:- use_module(library(lists)).
:- use_module(library(oaei/oaei_read)).
:- use_module(library(rdf/rdf_save)).





%! oaei_convert_rdf_to_tsv(+FromFile:atom, +ToFile:atom) is det.

oaei_convert_rdf_to_tsv(FromFile, ToFile):-
  oaei_load_rdf(FromFile, As),
  oaei_save_tsv(As, ToFile).



%! oaei_convert_tsv_to_rdf(+FromFile:atom, +ToFile:atom) is det.

oaei_convert_tsv_to_rdf(FromFile, ToFile):-
  oaei_load_tsv(FromFile, As),
  oaei_save_rdf(As, ToFile).



%! oaei_load_rdf(+In, -Alignments:ordset(pair(iri))) is det.

oaei_load_rdf(In, As):-
  rdf_read_from_graph(In, oaei_load_rdf0(As)).

oaei_load_rdf0(As, G):-
  aggregate_all(set(From-To), oaei_alignment(From, To, G), As).



%! oaei_load_tsv(+File:atom, -Alignments:ordset(pair(iri))) is det.

oaei_load_tsv(File, As):-
  tsv_read_file(File, Rows, [arity(2)]),
  aggregate_all(set(From-To), member(row(From,To), Rows), As).



%! oaei_save_rdf(+In, +Alignments:list(pair)) is det.

oaei_save_rdf(In, As):-
  rdf_write_to_graph(In, oaei_assert_alignments(As)).



%! oaei_save_tsv(+In, +Alignments:list(pair)) is det.

oaei_save_tsv(In, As):-
  setup_call_cleanup(
    open_any(In, write, Write, Close, []),
    forall(member(From-To, As), tsv_write_stream(Write, [row(From,To)])),
    close_any(Close)
  ).
