:- module(
  rdfs_fca_test,
  [
    rdfs_fca_test/1,      % ?Name
    rdfs_fca_test_file/1, % +File
    rdfs_fca_test_graph/1 % +G
  ]
   ).
:- reexport(library(fca/fca)).
:- reexport(library(q/rdf_print)).

/** <module> RDF FCA Test

@author Wouter Beek
@version 2017/01
*/

:- use_module(library(aggregate)).
:- use_module(library(fca/rdfs_fca)).
:- use_module(library(fca/rdfs_fca_viz)).
:- use_module(library(os/external_program)).
:- use_module(library(os/pdf)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(uri_ext)).

:- initialization(list_external_programs).





%! rdfs_fca_test(+Name) is semidet.
%! rdfs_fca_test(-Name) is multi.

rdfs_fca_test(dc) :-
  absolute_file_name(library('semweb/dc.rdfs'), File, [access(read)]),
  rdfs_fca_test_file(File).
rdfs_fca_test(eor) :-
  absolute_file_name(library('semweb/eor.rdfs'), File, [access(read)]),
  rdfs_fca_test_file(File).
rdfs_fca_test(owl) :-
  absolute_file_name(library('semweb/owl.owl'), File, [access(read)]),
  rdfs_fca_test_file(File).
rdfs_fca_test(rdfs) :-
  absolute_file_name(library('semweb/rdfs.rdfs'), File, [access(read)]),
  rdfs_fca_test_file(File).
rdfs_fca_test(Name) :-
  rdfs_fca_assert_graph(trp, Name, G),
  rdfs_fca_test_graph(G).



%! rdfs_fca_test_file(+File) is det.

rdfs_fca_test_file(File) :-
  rdf_call_on_graph(File, {Context}/[G,Meta,Meta]>>rdfs_fca_context(Context, G)),
  format(string(GLbl), "FCA for RDF file ~a", [File]),
  fca_viz0(Context, GLbl).



%! rdfs_fca_test_graph(+G) is semidet.

rdfs_fca_test_graph(G) :-
  rdfs_fca_context(Con, G),
  format(string(GLbl), "FCA for RDF graph ~a", [G]),
  fca_viz0(Con, GLbl).



%! rdfs_fca_assert_graph(+M, +Name, -G) is det.

rdfs_fca_assert_graph(M, number, G) :-
  fresh_uri(G, _),
  forall(
    between(1, 10, N),
    rdf_assert_number0(M, N, G)
  ),
  rdf_assert_instances(M, ex:'1',  [ex:'Odd',ex:'Square'],                 G),
  rdf_assert_instances(M, ex:'2',  [ex:'Even',ex:'Prime'],                 G),
  rdf_assert_instances(M, ex:'3',  [ex:'Odd',ex:'Prime'],                  G),
  rdf_assert_instances(M, ex:'4',  [ex:'Composite',ex:'Even',ex:'Square'], G),
  rdf_assert_instances(M, ex:'5',  [ex:'Odd',ex:'Prime'],                  G),
  rdf_assert_instances(M, ex:'6',  [ex:'Composite',ex:'Even'],             G),
  rdf_assert_instances(M, ex:'7',  [ex:'Odd',ex:'Prime'],                  G),
  rdf_assert_instances(M, ex:'8',  [ex:'Composite',ex:'Even'],             G),
  rdf_assert_instances(M, ex:'9',  [ex:'Composite',ex:'Odd',ex:'Square'],  G),
  rdf_assert_instances(M, ex:'10', [ex:'Composite',ex:'Even'],             G).





% HELPERS %

fca_viz0(Context, GLbl) :-
  fca_viz(
    Context,
    File,
    [
      attribute_label(dcg_rdf_print_term),
      concept_label(both),
      graph_label(GLbl),
      object_label(dcg_rdf_print_term)
    ]
  ),
  open_file(File).



rdf_assert_number0(M, N, G) :-
  atom_number(A, N),
  rdf_global_id(ex:A, Iri),
  rdf_assert_instance(M, Iri, ex:'Number', G),
  rdfs_assert_label(M, Iri, A, G).
