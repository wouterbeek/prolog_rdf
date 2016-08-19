:- module(
  rdf_fca_test,
  [
    rdf_fca_test/1,      % ?Name
    rdf_fca_test_file/1, % +File
    rdf_fca_test_graph/1 % +G
  ]
   ).
:- reexport(library(fca/fca)).
:- reexport(library(q/q_print)).

/** <module> RDF FCA Test

@author Wouter Beek
@version 2015/11-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(fca/rdf_fca)).
:- use_module(library(fca/rdf_fca_viz)).
:- use_module(library(q/qb)).
:- use_module(library(os/external_program)).
:- use_module(library(os/pdf)).
:- use_module(library(rdf/rdf__io)).

:- initialization(list_external_programs).





%! rdf_fca_test(+Name) is semidet.
%! rdf_fca_test(-Name) is multi.

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
  rdf_fca_assert_graph(trp, Name, G),
  rdf_fca_test_graph(G).



%! rdf_fca_test_file(+File) is det.

rdf_fca_test_file(File) :-
  rdf_call_on_graph(File, {Context}/[G,Meta,Meta]>>rdf_fca_context(Context, G)),
  format(string(GLbl), "FCA for RDF file ~a", [File]),
  fca_viz0(Context, GLbl).



%! rdf_fca_test_graph(+G) is semidet.

rdf_fca_test_graph(G) :-
  rdf_fca_context(Con, G),
  format(string(GLbl), "FCA for RDF graph ~a", [G]),
  fca_viz0(Con, GLbl).



%! rdf_fca_assert_graph(+M, +Name, -G) is det.

rdf_fca_assert_graph(M, number, G) :-
  rdf_new_graph(number, G),
  forall(between(1, 10, N), qb_number0(M, N, G)),
  qb_instances(M, ex:'1',  [ex:'Odd',ex:'Square'],                 G),
  qb_instances(M, ex:'2',  [ex:'Even',ex:'Prime'],                 G),
  qb_instances(M, ex:'3',  [ex:'Odd',ex:'Prime'],                  G),
  qb_instances(M, ex:'4',  [ex:'Composite',ex:'Even',ex:'Square'], G),
  qb_instances(M, ex:'5',  [ex:'Odd',ex:'Prime'],                  G),
  qb_instances(M, ex:'6',  [ex:'Composite',ex:'Even'],             G),
  qb_instances(M, ex:'7',  [ex:'Odd',ex:'Prime'],                  G),
  qb_instances(M, ex:'8',  [ex:'Composite',ex:'Even'],             G),
  qb_instances(M, ex:'9',  [ex:'Composite',ex:'Odd',ex:'Square'],  G),
  qb_instances(M, ex:'10', [ex:'Composite',ex:'Even'],             G).





% HELPERS %

fca_viz0(Context, GLbl) :-
  fca_viz(
    Context,
    File,
    [
      attribute_label(dcg_q_print_term),
      concept_label(both),
      graph_label(GLbl),
      object_label(dcg_q_print_term)
    ]
  ),
  open_pdf(File).



qb_number0(M, N, G) :-
  atom_number(A, N),
  rdf_global_id(ex:A, Iri),
  qb_instance(M, Iri, ex:'Number', G),
  qb_label(M, Iri, A, G).
