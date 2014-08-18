:- module(
  rdfs_proof,
  [
    rdfs_proof/5 % ?Subject:or([bnode,iri])
                 % ?Predicate:iri
                 % ?Object:or([bnode,iri,literal])
                 % +Graph:atom
                 % -ProofTree:compound
  ]
).

/** <module> RDFS proof

Deductive closure over RDFS classes,
calculated via backward chaining.

@author Wouter Beek
@version 2014/03, 2014/06
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(rdf_name)). % Meta-DCG.
:- use_module(plRdf(rdf_vocabulary)).

:- rdf_meta(rdfs_proof(r,r,o,+,-)).
:- rdf_meta(rdfs_proof(r,r,o,+,+,-)).
:- rdf_meta(rdfs_rule(+,r,r,o,r,r,o)).
:- rdf_meta(rdfs_rule(+,r,r,o,r,r,o,r,r,o)).

:- discontiguous(rdfs_rule/7).
:- discontiguous(rdfs_rule/10).



rdfs_proof(S, P, O, G, ProofTree):-
  load_rdf_vocabulary(_),
  rdfs_proof(S, P, O, G, [], ProofTree).

% Deduced via a rule with no antecedents, i.e. simple entailment.
rdfs_proof(S, P, O, G, H, d(se,rdf(S,P,O),[])):-
  rdf(S, P, O, G),
  \+ variant_member(rdf(_,S,P,O), H).
% Deduced via a rule with one antecedent.
rdfs_proof(S1, P1, O1, G, H, d(Name,rdf(S1,P1,O1),[Tree])):-
  rdfs_rule(Name, S1,P1,O1, S2,P2,O2),
  \+ variant_member(rdf(_,S2,P2,O2), H),
  rdfs_proof(S2, P2, O2, G, [rdf(Name,S1,P1,O1)|H], Tree).
% Deduced via a rule with two antecedents: order 1.
rdfs_proof(S1, P1, O1, G, H, d(Name,rdf(S1,P1,O1),[Tree1,Tree2])):-
  rdfs_rule(Name, S1,P1,O1, S2,P2,O2, S3,P3,O3),
  \+ variant_member(rdf(_,S2,P2,O2), H),
  rdfs_proof(S2, P2, O2, G, [rdf(Name,S1,P1,O1)|H], Tree1),
  \+ variant_member(rdf(_,S3,P3,O3), H),
  rdfs_proof(S3, P3, O3, G, [rdf(Name,S1,P1,O1)|H], Tree2).
/*% Deduced via a rule with two antecedents: order 2.
rdfs_proof(S1, P1, O1, G, H, d(Name,rdf(S1,P1,O1),[Tree1,Tree2])):-
  rdfs_rule(Name, S1,P1,O1, S2,P2,O2, S3,P3,O3),
  \+ variant_member(rdf(_,S3,P3,O3), H),
  rdfs_proof(S3, P3, O3, G, [rdf(Name,S1,P1,O1)|H], Tree1),
  \+ variant_member(rdf(_,S2,P2,O2), H),
  rdfs_proof(S2, P2, O2, G, [rdf(Name,S1,P1,O1)|H], Tree2).*/


rdfs_rule('RDFS-1', P,rdf:type,rdf:'Property',
  _,P,_).
rdfs_rule('RDFS-2', I,rdf:type,C,
  P,rdfs:domain,C,
  I,P,_).
rdfs_rule('RDFS-3', I,rdf:type,C,
  P,rdfs:range,C,
  _,P,I).
rdfs_rule('RDFS-4a', I,rdf:type,rdfs:'Resource',
  I,_,_).
rdfs_rule('RDFS-4a', I,rdf:type,rdfs:'Resource',
  _,_,I).
rdfs_rule('RDFS-5', P,rdfs:subPropertyOf,R,
  P,rdfs:subPropertyOf,Q,
  Q,rdfs:subPropertyOf,R).
rdfs_rule('RDFS-6', P,rdfs:subPropertyOf,P,
  P,rdf:type,rdf:'Property').
rdfs_rule('RDFS-7', X,P,Y,
  Q,rdfs:subPropertyOf,P,
  X,Q,Y).
rdfs_rule('RDFS-8', C,rdfs:subClassOf,rdfs:'Resource',
  C,rdf:type,rdfs:'Class').
rdfs_rule('RDFS-9', I,rdf:type,C,
  D,rdfs:subClassOf,C,
  I,rdf:type,D).
rdfs_rule('RDFS-10', C,rdfs:subClassOf,C,
  C,rdf:type,rdfs:'Class').
rdfs_rule('RDFS-11', C,rdfs:subClassOf,E,
  C,rdfs:subClassOf,D,
  D,rdfs:subClassOf,E).
rdfs_rule('RDFS-12', P,rdfs:subPropertyOf,rdfs:member,
  P,rdf:type,rdfs:'ContainerMembershipProperty').
rdfs_rule('RDFS-13', C,rdfs:subClassOf,rdfs:'Literal',
  C,rdf:type, rdfs:'Datatype').


variant_member(_, []):- !, fail.
variant_member(X, [H|_]):-
  X =@= H, !.
variant_member(X, [_|T]):-
  variant_member(X, T).

