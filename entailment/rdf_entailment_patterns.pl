:- module(
  rdf_entailment_patterns,
  [
    rdf_entailment_pattern/2, % +Premise:compound
                              % -Conclusion:compound
    rdf_entailment_pattern/3 % +Premise1:compound
                             % +Premise2:compound
                             % -Conclusion:compound
  ]
).

/** <module> RDF Entailment Patterns

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- discontiguous(rdf_entailment_pattern/2).
:- discontiguous(rdf_entailment_pattern/3).

:- rdf_meta(rdf_entailment_pattern(t,t)).
:- rdf_meta(rdf_entailment_pattern(t,t,t)).



% RDFS 2
rdf_entailment_pattern(
  rdf(P, rdfs:domain, Domain),
  rdf(S, P, _),
  rdf(S, rdf:type, Domain)
).

% RDFS 3
rdf_entailment_pattern(
  rdf(P, rdfs:range, Range),
  rdf(_, P, O),
  rdf(O, rdf:type, Range)
).

% RDFS 4a
rdf_entailment_pattern(
  rdf(S, _, _),
  rdf(S, rdf:type, rdfs:'Resource')
).

% RDFS 4b
rdf_entailment_pattern(
  rdf(_, _, O),
  rdf(O, rdf:type, rdfs:'Resource')
).

% RDFS 5
rdf_entailment_pattern(
  rdf(P1, rdfs:subPropertyOf, P2),
  rdf(P2, rdfs:subPropertyOf, P3),
  rdf(P1, rdfs:subPropertyOf, P3)
).

% RDFS 6
rdf_entailment_pattern(
  rdf(P, rdf:type, rdf:'Property'),
  rdf(P, rdfs:subPropertyOf, P)
).

% RDFS 7
rdf_entailment_pattern(
  rdf(P1, rdfs:subPropertyOf, P2),
  rdf(S, P1, O),
  rdf(S, P2, O)
).

% RDFS 8
rdf_entailment_pattern(
  rdf(C, rdf:type, rdfs:'Class'),
  rdf(C, rdfs:subClassOf, rdfs:'Resource')
).

% RDFS 9
rdf_entailment_pattern(
  rdf(C1, rdfs:subClassOf, C2),
  rdf(I, rdf:type, C1),
  rdf(I, rdf:type, C2)
).

% RDFS 10
rdf_entailment_pattern(
  rdf(C, rdf:type, rdfs:'Class'),
  rdf(C, rdfs:subClassOf, C)
).

% RDFS 11
rdf_entailment_pattern(
  rdf(C1, rdfs:subClassOf, C2),
  rdf(C2, rdfs:subClassOf, C3),
  rdf(C1, rdfs:subClassOf, C3)
).

% RDFS 12
rdf_entailment_pattern(
  rdf(P, rdf:type, rdfs:'ContainerMembershipProperty'),
  rdf(P, rdfs:subPropertyOg, rdfs:member)
).

% RDFS 13
rdf_entailment_pattern(
  rdf(D, rdf:type, rdfs:'Datatype'),
  rdf(D, rdfs:subClassOf, rdfs:'Literal')
).

