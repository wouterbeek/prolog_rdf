:- module(
  rdf_fca_test,
  [
    rdf_fca_test/1, % ?Name:atom
    rdf_fca_test_file/1, % +File:atom
    rdf_fca_test_graph/1 % +Graph:atom
  ]
   ).
:- reexport(library(fca/fca)).
:- reexport(library(rdf/rdf_print)).

/** <module> RDF FCA Test

@author Wouter Beek
@version 2015/11-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(fca/rdf_fca)).
:- use_module(library(fca/rdf_fca_viz)).
:- use_module(library(os/external_program)).
:- use_module(library(os/pdf)).
:- use_module(library(rdf/rdfio)).

:- initialization(list_external_programs).





%! rdf_fca_test(+Name:atom) is semidet.
%! rdf_fca_test(-Name:atom) is multi.

rdf_fca_test(dc) :-
  absolute_file_name(library('semweb/dc.rdfs'), File, [access(read)]),
  rdf_fca_test_file(File).
rdf_fca_test(eor) :-
  absolute_file_name(library('semweb/eor.rdfs'), File, [access(read)]),
  rdf_fca_test_file(File).
rdf_fca_test(owl) :-
  absolute_file_name(library('semweb/owl.owl'), File, [access(read)]),
  rdf_fca_test_file(File).
rdf_fca_test(rdfs) :-
  absolute_file_name(library('semweb/rdfs.rdfs'), File, [access(read)]),
  rdf_fca_test_file(File).
rdf_fca_test(Name) :-
  rdf_fca_assert_graph(Name, G),
  rdf_fca_test_graph(G).



%! rdf_fca_test_file(+File:atom) is det.

rdf_fca_test_file(File) :-
  rdf_call_on_graph(File, rdf_fca_context0(Context)),
  format(string(GLbl), "FCA for RDF file ~a", [File]),
  fca_viz0(Context, GLbl).

rdf_fca_context0(Context, _, G) :-
  rdf_fca_context(Context, G).



%! rdf_fca_test_graph(+Graph:atom) is semidet.

rdf_fca_test_graph(G) :-
  rdf_fca_context(Con, G),
  format(string(GLbl), "FCA for RDF graph ~a", [G]),
  fca_viz0(Con, GLbl).



%! rdf_fca_assert_graph(+Name:atom, -Graph:atom) is det.

rdf_fca_assert_graph(number, G) :-
  rdf_new_graph(number, G),
  forall(between(1, 10, N), rdf_assert_number(N, G)),
  rdf_assert_instance(ex:'1',  [ex:'Odd',ex:'Square'],                 G),
  rdf_assert_instance(ex:'2',  [ex:'Even',ex:'Prime'],                 G),
  rdf_assert_instance(ex:'3',  [ex:'Odd',ex:'Prime'],                  G),
  rdf_assert_instance(ex:'4',  [ex:'Composite',ex:'Even',ex:'Square'], G),
  rdf_assert_instance(ex:'5',  [ex:'Odd',ex:'Prime'],                  G),
  rdf_assert_instance(ex:'6',  [ex:'Composite',ex:'Even'],             G),
  rdf_assert_instance(ex:'7',  [ex:'Odd',ex:'Prime'],                  G),
  rdf_assert_instance(ex:'8',  [ex:'Composite',ex:'Even'],             G),
  rdf_assert_instance(ex:'9',  [ex:'Composite',ex:'Odd',ex:'Square'],  G),
  rdf_assert_instance(ex:'10', [ex:'Composite',ex:'Even'],             G).





% HELPERS %

fca_viz0(Context, GLbl) :-
  fca_viz(
    Context,
    File,
    [
      attribute_label(dcg_print_term),
      concept_label(both),
      graph_label(GLbl),
      object_label(dcg_print_term)
    ]
  ),
  open_pdf(File).



rdf_assert_number(N, G) :-
  atom_number(A, N),
  rdf_global_id(ex:A, Iri),
  rdf_assert_instance(Iri, ex:'Number', G),
  rdfs_assert_label(Iri, A, G).
