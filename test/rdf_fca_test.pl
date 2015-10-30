:- module(
  rdf_fca_test,
  [
    rdf_fca_test_context/2, % +Id:positive_integer
                            % -Context:compound
    rdf_fca_test_graph/2, % +Id:positive_integer
                          % -Graph:atom
    rdf_fca_test_hasse/2 % +Id:positive_integer
                         % -Hasse:ugraph
  ]
   ).
:- reexport(library(fca/fca)).
:- reexport(library(rdf/rdf_print)).

/** <module> RDF FCA Test

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(fca/bordat)).
:- use_module(library(fca/rdf_fca)).
:- use_module(library(profile/profile_rdf)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdfs/rdfs_build)).

:- rdf_register_prefix(ex, 'http://www.example.org/', [keep(true)]).





%! rdf_fca_test_context(+Id:positive_integer, -Context:compound) is det.

rdf_fca_test_context(N, Context):-
  atomic_list_concat([fca,N], /, G0),
  rdf_new_graph(G0, G),
  rdf_fca_test_graph(N, G),
  rdf_fca_context(G, Context).



%! rdf_fca_test_graph(+Id:positive_integer, -Graph:atom) is det.

rdf_fca_test_graph(1, G):-
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



%! rdf_fca_test_hasse(+Id:positive_integer, -Hasse:ugraph) is det.

rdf_fca_test_hasse(N, Hasse):-
  rdf_fca_test_context(N, Context),
  fca_hasse(Context, Hasse).





% HELPERS %

rdf_assert_number(N, G):-
  atom_number(A, N),
  rdf_global_id(ex:A, Iri),
  rdf_assert_instance(Iri, ex:'Number', G),
  rdfs_assert_label(Iri, A, G).
