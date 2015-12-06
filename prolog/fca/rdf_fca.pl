:- module(
  rdf_fca,
  [
    rdf_fca_from_file/2, % +File:atom
                         % -Context:compound
    rdf_fca_from_graph/2 % +Graph:atom
                         % -Context:compound
  ]
).

/** <module> FCA for RDF

@author Wouter Beek
@version 2015/11-2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdfs/rdfs_read)).





%! rdf_fca_from_file(+File:atom, -Context:compound) is det.

rdf_fca_from_file(File, Context):-
  rdf_new_graph(fca, G),
  rdf_load_file(File, [graph(G)]),
  rdf_fca_from_graph(G, Context).



%! rdf_fca_from_graph(+Graph:atom, -Context:compound) is det.

rdf_fca_from_graph(G, context(Os,As,rdf_fca:rdf_instance0(G))):-
  aggregate_all(set(O), (rdf_subject(O), rdf(O, _, _, G)), Os),
  aggregate_all(set(A), rdfs_class(A, G), As).
rdf_instance0(G, O, A):- rdf_instance(O, A, G).
