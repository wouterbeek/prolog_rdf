:- module(
  oaei_file,
  [
    oaei_convert_rdf_to_tsv/2, % +In
                               % +Out
    oaei_convert_tsv_to_rdf/2, % +In
                               % +Out
    oaei_load_rdf/2, % +In
                     % -Alignments:ugraph
    oaei_load_rdf/3, % +In
                     % -Alignments:ugraph
                     % +Options:list(compound)
    oaei_load_tsv/2, % +In
                     % -Alignments:ugraph
    oaei_save_rdf/2, % +Out
                     % +Alignments:list(pair))
    oaei_save_tsv/2 % +Out
                    % +Alignments:list(pair))
  ]
).

/** <module> Ontology Alignment Evaluation Initiative (OAEI)

@author Wouter Beek
@version 2015/10, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(csv_ext)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(oaei/oaei_build)).
:- use_module(library(oaei/oaei_read)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_load)).

:- predicate_options(oaei_load_rdf/3, 3, [
     pass_to(rdf_call_on_graph/3, 3)
   ]).





%! oaei_convert_rdf_to_tsv(+In, +Out) is det.

oaei_convert_rdf_to_tsv(FromFile, ToFile) :-
  oaei_load_rdf(FromFile, As),
  oaei_save_tsv(As, ToFile).



%! oaei_convert_tsv_to_rdf(+In, +Out) is det.

oaei_convert_tsv_to_rdf(FromFile, ToFile) :-
  oaei_load_tsv(FromFile, As),
  oaei_save_rdf(As, ToFile).



%! oaei_load_rdf(+In, -Alignments:ordset(pair(iri))) is det.

oaei_load_rdf(In, As) :-
  oaei_load_rdf(In, As, []).


%! oaei_load_rdf(+In, -Alignments:ordset(pair(iri)), +Options:list(compound)) is det.

oaei_load_rdf(In, As, Opts) :-
  rdf_call_on_graph(In, oaei_load_rdf0(As), Opts).

oaei_load_rdf0(As, _) :-
  aggregate_all(set(From-To), oaei_alignment(From, To), As).



%! oaei_load_tsv(+In, -Alignments:ordset(pair(iri))) is det.

oaei_load_tsv(File, As) :-
  tsv_read_file(File, Rows, [arity(2)]),
  aggregate_all(set(From-To), member(row(From,To), Rows), As).



%! oaei_save_rdf(+Out, +Alignments:list(pair)) is det.

oaei_save_rdf(In, As) :-
  rdf_write_to_graph(In, \G^oaei_assert_alignments(As, G)).



%! oaei_save_tsv(+Out, +Alignments:list(pair)) is det.

oaei_save_tsv(In, As) :-
  setup_call_cleanup(
    open_any2(In, write, Write, Close_0),
    forall(member(From-To, As), tsv_write_stream(Write, [row(From,To)])),
    close_any2(Close_0)
  ).
