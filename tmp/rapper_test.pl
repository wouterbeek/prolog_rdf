:- module(
  rapper_test,
  [
    rapper_test/0
  ]
).

/** <module> Rapper test

User Rapper to test whether LOD Laundromat data can be parsed.

@author Wouter Beek
@version 2015/03, 2016/01, 2016/04
*/

:- use_module(library(csv_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(os/archive_ext)).
:- use_module(library(service/ll_api)).

:- dynamic
    rapper_result/4.





%! rapper_test is det.

rapper_test:-
  absolute_file_name(data('rapper.csv'), File, [access(write)]),
  call_to_steam(File, rapper_test0).

rapper_test0(_, Out) :-
  forall(
    (
      doc_size(50000, 100000, Doc),
      metadata(Doc, llo:statementsType, "triples"^^xsd:string)
    ),
    (
      metadata(Doc, llo:triples, Triples1^^xsd:integer),
      doc_download(Doc, Iri),
      rapper_test(Iri, Doc, Triples2, Triples3),
      csv_write_stream(Out, [row(Triples1,Triples2,Triples3,Doc,Iri)]),
      flush_output(Out)
    )
  ).



%! rapper_test(+Iri, +Doc, -DirtyTriples, -CleanTriples) is det.

rapper_test(Iri, Doc, DirtyTriples, CleanTriples) :-
  doc_name(Doc, Name),

  % Dirty.
  metadata(Doc, llo:url, DirtyIri),
  atomic_list_concat([Name,dirty], '_', DirtyName),
  rapper_test(DirtyIri, DirtyName, DirtyTriples),

  % Clean.
  atomic_list_concat([Name,clean], '_', CleanName),
  rapper_test(Iri, CleanName, CleanTriples).



%! rapper_test(+Iri, +Name, -NumTriples) is det.

rapper_test(Iri, Name, NumTriples) :-
  absolute_file_name(Name, File, [access(write)]),
  catch(
    call_on_strean(Iri, rapper_test0(NumTriples)),
    E,
    (print_message(warning, exception(E)), NTriples = 0)
  ).

rapper_test0(NumTriples, In, _, _) :-
  Opts = [
    count(true),
    guess(true),
    ignore_errors(true),
    triples(NumTriples)
  ],
  rapper(In, Opts).





% MESSAGES %

:- multifile
    prolog:message//1.

prolog:message(exception(Ex)) --> ['~w'-E].
