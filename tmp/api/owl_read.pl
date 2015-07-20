:- module(
  owl_read,
  [
    owl_class_equiv/2, % ?Class1:iri
                       % ?Class2:iri
    owl_class_disjoint/2, % ?Class1:iri
                          % ?Class2:iri
    owl_id/2, % ?Term1:iri
              % ?Term2:iri
/*
    owl_id/3, % ?Term1:iri
              % ?Term2:iri
              % ?Graph:atom
    owl_id/4, % ?Term1:iri
              % ?Graph1:atom
              % ?Term2:iri
              % ?Graph2:atom
    owl_id/5, % ?Term1:iri
              % ?Graph1:atom
              % ?Term2:iri
              % ?Graph2:atom
              % ?LinkGraph:atom
*/
    owl_ids/2, % +Term:iri
               % -IdTerms:ordset(iri)
    owl_label_or_term/2 % +Term:iri
                        % -Label:atom
  ]
).

/** <module> OWL read

Predicates for reading from OWL data.

@author Wouter Beek
@version 2013-2015
*/

:- use_module(library(aggregate)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs), except([rdfs_label/3])).

:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(owl_class_equiv(r,r)).
:- rdf_meta(owl_class_disjoint(r,r)).
:- rdf_meta(owl_id(r,r)).
:- rdf_meta(owl_ids(r,-)).
:- rdf_meta(owl_label_or_term(r,-)).





%! owl_class_equiv(+Class1:iri, +Class2:iri) is semidet.
%! owl_class_equiv(+Class1:iri, -Class2:iri) is nondet.
%! owl_class_equiv(-Class1:iri, +Class2:iri) is nondet.

owl_class_equiv(C1, C2):-
  rdf_reachable(C1, owl:equivalentClass, C2).



%! owl_class_disjoint(+Class1:iri, +Class2:iri) is semidet.
%! owl_class_disjoint(+Class1:iri, -Class2:iri) is nondet.
%! owl_class_disjoint(-Class1:iri, +Class2:iri) is nondet.

owl_class_disjoint(C1, C2):-
  rdf_reachable(C1, owl:disjointWith, C2).



%! owl_id(+Term1:iri, +Term2:iri) is semidet.
%! owl_id(+Term1:iri, -Term2:iri) is multi.
%! owl_id(-Term1:iri, +Term2:iri) is multi.

owl_id(T1, T2):-
  rdf_reachable(T1, owl:sameAs, T2).



/*
@tbd The following require `rdf_reachable(S,P,O,G)`.

owl_id(R1, R2, G):-
  rdf(R1, owl:sameAs, R2, G).



owl_id(T1, G1, T2, G2):-
  owl_id(T1, G1, T2, G2, _LinkG).



owl_id(T1, G1, T2, G2, LinkG):-
  rdf(T1, owl:sameAs, T2, LinkG),
  rdf(T1, _P1, _O1, G1),
  rdf(T2, _P2, _O2, G2).
owl_id(T, G, T, G, _).
*/



%! owl_ids(+Term:iri, -IdenticalTerms:ordset(iri)) is det.
% Returns the identity set for the given IRI.

owl_ids(T0, ClT):-
  aggregate_all(set(T), owl_id(T0, T), ClT).



%! owl_label_or_term(+Term:iri, -Label:atom) is nondet.

owl_label_or_term(T0, L):-
  rdf_reachable(T0, owl:sameAs, T),
  rdfs_label_or_term(T, L).
