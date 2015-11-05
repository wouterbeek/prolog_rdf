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
@version 2015/11
*/

:- use_module(library(aggregate)).
:- use_module(library(fca/fca_export)).
:- use_module(library(fca/rdf_fca)).
:- use_module(library(os/external_program)).
:- use_module(library(os/pdf)).
:- use_module(library(profile/profile_rdf)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(rdfs/rdfs_build)).

:- initialization(list_external_programs).

:- rdf_register_prefix(ex, 'http://www.example.org/', [keep(true)]).





%! rdf_fca_test(+Name:atom) is semidet.
%! rdf_fca_test(-Name:atom) is multi.

rdf_fca_test(dc):-
  absolute_file_name(library('semweb/dc.rdfs'), File, [access(read)]),
  rdf_fca_test_file(File).
rdf_fca_test(Name):-
  rdf_fca_assert_graph(Name, G),
  rdf_fca_test_graph(G).



%! rdf_fca_test_file(+File:atom) is det.

rdf_fca_test_file(File):-
  rdf_fca_from_file(File, Context),
  format(string(GLbl), "FCA for RDF file ~a", [File]),
  fca_export0(Context, GLbl).



%! rdf_fca_test_graph(+Graph:atom) is semidet.

rdf_fca_test_graph(G):-
  rdf_fca_from_graph(G, Context),
  format(string(GLbl), "FCA for RDF graph ~a", [G]),
  fca_export0(Context, GLbl).



%! rdf_fca_assert_graph(+Name:atom, -Graph:atom) is det.

rdf_fca_assert_graph(number, G):-
  rdf_new_graph(number, G),
  forall(between(1, 10, N), rdf_assert_number(N, G)),
  rdf_assert(ex:'1',  rdf:type, ex:'Odd',       G),
  rdf_assert(ex:'1',  rdf:type, ex:'Square',    G),
  rdf_assert(ex:'2',  rdf:type, ex:'Even',      G),
  rdf_assert(ex:'2',  rdf:type, ex:'Prime',     G),
  rdf_assert(ex:'3',  rdf:type, ex:'Odd',       G),
  rdf_assert(ex:'3',  rdf:type, ex:'Prime',     G),
  rdf_assert(ex:'4',  rdf:type, ex:'Composite', G),
  rdf_assert(ex:'4',  rdf:type, ex:'Even',      G),
  rdf_assert(ex:'4',  rdf:type, ex:'Quare',     G),
  rdf_assert(ex:'5',  rdf:type, ex:'Odd',       G),
  rdf_assert(ex:'5',  rdf:type, ex:'Prime',     G),
  rdf_assert(ex:'6',  rdf:type, ex:'Composite', G),
  rdf_assert(ex:'6',  rdf:type, ex:'Even',      G),
  rdf_assert(ex:'7',  rdf:type, ex:'Odd',       G),
  rdf_assert(ex:'7',  rdf:type, ex:'Prime',     G),
  rdf_assert(ex:'8',  rdf:type, ex:'Composite', G),
  rdf_assert(ex:'8',  rdf:type, ex:'Even',      G),
  rdf_assert(ex:'9',  rdf:type, ex:'Composite', G),
  rdf_assert(ex:'9',  rdf:type, ex:'Odd',       G),
  rdf_assert(ex:'9',  rdf:type, ex:'Square',    G),
  rdf_assert(ex:'10', rdf:type, ex:'Composite', G),
  rdf_assert(ex:'10', rdf:type, ex:'Even',      G).





% HELPERS %

fca_export0(Context, GLbl):-
  fca_export(
    Context,
    File,
    [
      attribute_label(rdf_print_term),
      concept_label(both),
      graph_label(GLbl),
      object_label(rdf_print_term)
    ]
  ),
  open_pdf(File).



rdf_assert_number(N, G):-
  atom_number(A, N),
  rdf_global_id(ex:A, Iri),
  rdf_assert_instance(Iri, ex:'Number', G),
  rdfs_assert_label(Iri, A, G).
