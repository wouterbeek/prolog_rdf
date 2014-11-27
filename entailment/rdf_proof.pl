:- module(
  rdf_proof,
  [
    rdf_proof/5 % +Subject:or([bnode,iri])
                % +Predicate:iri
                % +Object:rdf_term
                % ?Graph:atom
                % -ProofTree:pair
  ]
).

/** <module> RDFS proof

Deductive closure over RDFS classes,
calculated via backward chaining.

@author Wouter Beek
@version 2014/03, 2014/06, 2014/11
*/

:- use_module(library(lists), except([delete/3])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(term/rdf_instance)).

:- rdf_meta(rdf_proof(r,r,o,?,-)).
:- rdf_meta(rdf_rule(-,t,t)).
:- rdf_meta(rdf_rule(-,t,t,t)).

:- discontiguous(rdf_rule/3).
:- discontiguous(rdf_rule/4).



%! rdf_proof(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom,
%!   -ProofTree:pair
%! ) is nondet.

rdf_proof(S, P, O, G, ProofTree):-
  rdf_proof(rdf(S,P,O,G), [], ProofTree).

%! rdf_proof(
%!   +Triple:compound,
%!   +HistoryTriples:list(compound),
%!   -ProofTree:pair
%! ) is det.

% Deduced via a rule with no antecedents, i.e. simple entailment.
rdf_proof(rdf(S,P,O,G), Triples, deduction(rdf(S,P,O),se)-[]):-
  rdf(S, P, O, G),
  \+ rdf_triple_variant_member(rdf(S,P,O), Triples).
% Deduced via a rule with one antecedent.
rdf_proof(rdf(S1,P1,O1,G), Triples, deduction(rdf(S1,P1,O1),Name)-[Subtree]):-
  rdf_rule(Name, rdf(S1,P1,O1), rdf(S2,P2,O2)),
  \+ rdf_triple_variant_member(rdf(S2,P2,O2), Triples),
  rdf_proof(rdf(S2,P2,O2,G), [rdf(S1,P1,O1)|Triples], Subtree).
% Deduced via a rule with two antecedents: order 1.
rdf_proof(
  rdf(S1,P1,O1,G),
  Triples,
  deduction(rdf(S1,P1,O1),Name)-[Subtree1,Subtree2]
):-
  rdf_rule(Name, rdf(S1,P1,O1), rdf(S2,P2,O2), rdf(S3,P3,O3)),
  \+ rdf_triple_variant_member(rdf(S2,P2,O2), Triples),
  \+ rdf_triple_variant_member(rdf(S3,P3,O3), Triples),
  rdf_proof(rdf(S2,P2,O2,G), [rdf(S1,P1,O1)|Triples], Subtree1),
  rdf_proof(rdf(S3,P3,O3,G), [rdf(S1,P1,O1)|Triples], Subtree2).



rdf_rule('RDFS-1',
  rdf(P,rdf:type,rdf:'Property'),
  rdf(_,P,_)
).
rdf_rule('RDFS-2',
  rdf(I,rdf:type,C),
  rdf(P,rdfs:domain,C),
  rdf(I,P,_)
).
rdf_rule('RDFS-3',
  rdf(I,rdf:type,C),
  rdf(P,rdfs:range,C),
  rdf(_,P,I)
).
rdf_rule('RDFS-4a',
  rdf(I,rdf:type,rdfs:'Resource'),
  rdf(I,_,_)
).
rdf_rule('RDFS-4a',
  rdf(I,rdf:type,rdfs:'Resource'),
  rdf(_,_,I)
).
rdf_rule('RDFS-5',
  rdf(P,rdfs:subPropertyOf,R),
  rdf(P,rdfs:subPropertyOf,Q),
  rdf(Q,rdfs:subPropertyOf,R)
).
rdf_rule('RDFS-6',
  rdf(P,rdfs:subPropertyOf,P),
  rdf(P,rdf:type,rdf:'Property')
).
rdf_rule('RDFS-7',
  rdf(X,P,Y),
  rdf(Q,rdfs:subPropertyOf,P),
  rdf(X,Q,Y)
).
rdf_rule('RDFS-8',
  rdf(C,rdfs:subClassOf,rdfs:'Resource'),
  rdf(C,rdf:type,rdfs:'Class')
).
rdf_rule('RDFS-9',
  rdf(I,rdf:type,C),
  rdf(D,rdfs:subClassOf,C),
  rdf(I,rdf:type,D)
).
rdf_rule('RDFS-10',
  rdf(C,rdfs:subClassOf,C),
  rdf(C,rdf:type,rdfs:'Class')
).
rdf_rule('RDFS-11',
  rdf(C,rdfs:subClassOf,E),
  rdf(C,rdfs:subClassOf,D),
  rdf(D,rdfs:subClassOf,E)
).
rdf_rule('RDFS-12',
  rdf(P,rdfs:subPropertyOf,rdfs:member),
  rdf(P,rdf:type,rdfs:'ContainerMembershipProperty')
).
rdf_rule('RDFS-13',
  rdf(C,rdfs:subClassOf,rdfs:'Literal'),
  rdf(C,rdf:type,rdfs:'Datatype')
).





% HELPERS

%! rdf_triple_variant_member(
%!   +Triple:compound,
%!   +Triples:list(compound)
%! ) is semidet.

rdf_triple_variant_member(rdf(S1,P,O1), Triples):-
  % Export the predicate term inside the triple compound terms
  %  for more efficient matching.
  member(rdf(S2,P,O2), Triples),
  rdf_triple_variant(rdf(S1,P,O1), rdf(S2,P,O2)), !.
