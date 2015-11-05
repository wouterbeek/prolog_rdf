:- module(
  rdf_fca,
  [
    rdf_fca_context/2 % +Graph:atom
                        % -Context:compound
  ]
).

/** <module> Formal Concept Analysis for RDF

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(aggregate)).
:- use_module(library(profile/profile_rdf)).
:- use_module(library(rdf/rdf_read)).





%! rdf_fca_context(+Graph:atom, -Context:compound) is det.

rdf_fca_context(G, context(Os,As,rdf_fca:rdf_instance0(G))):-
  aggregate_all(set(O), (user:rdf_subject(O), user:rdf(O, _, _, G)), Os),
  aggregate_all(set(A), rdfs_class(A, G), As).
  

rdf_instance0(G, O, A):-
  rdf_instance(O, A, G).


rdfs_class(C, G):-
  user:rdf(C, rdf:type, rdfs:'Class', G).
rdfs_class(C, G):-
  user:rdf(_, rdf:type, C, G).
rdfs_class(C, G):-
  user:rdf(_, rdfs:domain, C, G).
rdfs_class(C, G):-
  user:rdf(_, rdfs:range, C, G).
rdfs_class(C, G):-
  user:rdf(C, rdfs:subClassOf, _, G).
rdfs_class(C, G):-
  user:rdf(_, rdfs:subClassOf, C, G).
