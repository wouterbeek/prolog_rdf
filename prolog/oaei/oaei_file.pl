:- module(
  oaei_file,
  [
    oaei_convert_rdf_to_tsv/2, % +Source, +Sink
    oaei_convert_tsv_to_rdf/2, % +Source, +Sink
    oaei_load_rdf/2,           % +Source, -Alignments
    oaei_load_rdf/3,           % +Source, -Alignments, +Opts
    oaei_load_tsv/2,           % +Source, -Alignments
    oaei_save_rdf/2,           % +Sink,   +Alignments
    oaei_save_tsv/2            % +Sink,   +Alignments
  ]
).

/** <module> Ontology Alignment Evaluation Initiative (OAEI)

@author Wouter Beek
@version 2015/10, 2015/12-2016/01, 2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(csv_ext)).
:- use_module(library(lists)).
:- use_module(library(oaei/oaei_build)).
:- use_module(library(oaei/oaei_read)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_save)).
:- use_module(library(yall)).





%! oaei_convert_rdf_to_tsv(+Source, +Sink) is det.

oaei_convert_rdf_to_tsv(Source, Sink) :-
  oaei_load_rdf(Source, Alignments),
  oaei_save_tsv(Alignments, Sink).



%! oaei_convert_tsv_to_rdf(+Source, +Sink) is det.

oaei_convert_tsv_to_rdf(Source, Sink) :-
  oaei_load_tsv(Source, Alignments),
  oaei_save_rdf(Alignments, Sink).



%! oaei_load_rdf(+Source, -Alignments:ordset(pair(iri))) is det.
%! oaei_load_rdf(+Source, -Alignments:ordset(pair(iri)), +Opts) is det.

oaei_load_rdf(Source, Alignments) :-
  oaei_load_rdf(Source, Alignments, []).


oaei_load_rdf(Source, Alignments, Opts) :-
  rdf_call_on_graph(Source, oaei_load_rdf0(Alignments), Opts).

oaei_load_rdf0(Alignments, _, _, _) :-
  aggregate_all(set(From-To), oaei_alignment(From, To), Alignments).



%! oaei_load_tsv(+Source, -Alignments:ordset(pair(iri))) is det.

oaei_load_tsv(Source, Alignments) :-
  tsv_read_file(Source, Rows, [arity(2)]),
  aggregate_all(set(From-To), member(row(From,To), Rows), Alignments).



%! oaei_save_rdf(+Sink, +Alignments:list(pair)) is det.

oaei_save_rdf(Sink, Alignments) :-
  rdf_call_to_graph(Sink, [G]>>oaei_assert_alignments(Alignments, G)).



%! oaei_save_tsv(+Sink, +Alignments:list(pair)) is det.

oaei_save_tsv(Sink, Alignments) :-
  call_to_stream(Sink, oaei_save_tsv0(Alignments)).

oaei_save_tsv0(Alignments, Out, _, _) :-
  forall(member(From-To, Alignments), tsv_write_stream(Out, [row(From,To)])).
