:- module(
  rdfs_ent,
  [
    rdf:axiom/2, % ?Regime:atom
                 % ?Axiom:compound
    rdf:explanation/3, % ?Regime:atom
                       % ?Rule:atom
                       % ?Explanation:atom
    rdf:rule/5 % ?Regime:atom
               % ?Rule:atom
               % ?Premises:list(compound)
               % ?Conclusion:compound
               % ?Graph:atom
  ]
).

/** <module> RDFS entailment

Specification of entailment rules for RDFS.

@author Wouter Beek
@see rdf-mt 1.1 (2014)
@version 2013/08-2013/09, 2014/06-2014/07
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf_ent(rdf_bnode_map)).
:- use_module(plRdf_term(rdf_plain_literal)).
:- use_module(plRdf_term(rdf_term)).

:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

%! rdf:axiom(?Regime:atom, ?Axiom:compound) is nondet.

:- discontiguous(rdf:axiom/2).
:- multifile(rdf:axiom/2).
:- rdf_meta(rdf:axiom(?,t)).

%! rdf:explanation(?Regime:atom, ?Rule:atom, ?Explanation:atom) is nondet.

:- discontiguous(rdf:explanation/3).
:- multifile(rdf:explanation/3).

%! rdf:rule(
%!   ?Regime:atom,
%!   ?Rule:atom,
%!   ?Premises:list(compound),
%!   ?Conclusion:compound,
%!   ?Graph:atom
%! ) is nondet.

:- discontiguous(rdf:rule/5).
:- multifile(rdf:rule/5).
:- rdf_meta(rdf:rule(?,?,t,t,?)).

%! rdf:regime(?Regime:atom) is nondet.

:- discontiguous(rdf:regime/1).
:- multifile(rdf:regime/1).



rdf:regime(rdfs).


% [gl] Literal instantiation rule
%      This ensures that every triple that contains a literal and
%      its similar triple that contains the allocated blank node
%      (according to the literal generation rule [lg])
%      are derivable from each other.

rdf:explanation(
  rdfs,
  gl,
  'The literal instantiation rule ensures that every triple that contains\c
   a literal and the triple that only differs in that it contains\c
   the blank node allocated to that triple, are derivable from each other.'
).

rdf:rule(rdfs, gl, [rdf(S,P,B)], rdf(S,P,Lit), G):-
  rdf(S, P, B, G),
  % If the object term is not a blank node,
  % then we do not have to search the blank node-literal mapping.
  rdf_is_bnode(B),

  % Not every blank node that is an object term in some triple
  % is a generalization for a literal.
  % Therefore, it has to occur in the mapping established by rule [lg].
  bnode_to_term(G, B, Lit),
  rdf_is_literal(Lit).


% [rdfs1] Literals are instances of =|rdfs:'Literal'|=.

rdf:explanation(
  rdfs,
  rdfs1,
  'Literal terms belong to the extension of the RDFS literal class.'
).

rdf:rule(
  rdfs,
  rdfs1,
  [rdf(S,P,PlainLit)],
  rdf(B,rdf:type,rdfs:'Literal'),
  G
):-
  rdf(S, P, PlainLit, G),
  rdf_plain_literal(PlainLit),

  term_to_bnode(G, PlainLit, B).


% [rdfs2] Class membership through domain restriction.

rdf:explanation(rdfs, rdfs2, 'Class membership through domain restriction.').

rdf:rule(
  rdfs,
  rdfs2,
  [rdf(P,rdfs:domain,C),rdf(S,P,O)],
  rdf(S,rdf:type,C),
  G
):-
  rdf(P, rdfs:domain, C, G),
  rdf_iri(P),
  rdf(S, P, O, G).


% [rdfs3] Class membership through range restriction.

rdf:explanation(rdfs, rdfs3, 'Class membership through range restriction.').

rdf:rule(rdfs, rdfs3, [rdf(P,rdfs:range,C),rdf(S,P,O)], rdf(O,rdf:type,C), G):-
  rdf(P, rdfs:range, C, G),
  rdf_iri(P),
  rdf(S, P, O, G),
  \+ rdf_is_literal(O).


% [rdfs4a] Subject terms are resources.

rdf:explanation(
  rdfs,
  rdfs4a,
  'Terms that occur in the subject position are RDFS resources.'
).

rdf:rule(rdfs, rdfs4a, [rdf(S,P,O)], rdf(S,rdf:type,rdfs:'Resource'), G):-
  rdf(S, P, O, G).


% [rdfs4b] Object terms are resources.

rdf:explanation(
  rdfs,
  rdfs4b,
  'Terms that occur in the object position are RDFS resources.'
).

rdf:rule(rdfs, rdfs4b, [rdf(S,P,O)], rdf(O,rdf:type,rdfs:'Resource'), G):-
  rdf(S, P, O, G),
  \+ rdf_is_literal(O).


% [rdfs5] Transitive closure of the property hierarchy relation.

rdf:explanation(
  rdfs,
  rdfs5,
  'Transitive closure of the property hierarchy relation.'
).

rdf:rule(
  rdfs,
  rdfs5,
  [rdf(P1,rdfs:subPropertyOf,P2),rdf(P2,rdfs:subPropertyOf,P3)],
  rdf(P1,rdfs:subPropertyOf,P3),
  G
):-
  rdf(P1, rdfs:subPropertyOf, P2, G),
  % `P2` is automatically constrained to blank nodes and IRIs,
  % since is must appear as the subject term of some triple.
  rdf(P2, rdfs:subPropertyOf, P3, G).


% [rdfs6] Reflexivity of the property hierarchy relation.

rdf:explanation(
  rdfs,
  rdfs6,
  'Reflexivity of the property hierarchy relation.'
).

rdf:rule(
  rdfs,
  rdfs6,
  [rdf(P,rdf:type,rdf:'Property')],
  rdf(P,rdfs:subPropertyOf,P),
  G
):-
  rdf(P, rdf:type, rdf:'Property', G).


% [rdfs7] Using the property hierarchy.

rdf:explanation(rdfs, rdfs7, 'Using the property hierarchy.').

rule(
  rdfs,
  rdfs7,
  [rdf(P1,rdfs:subPropertyOf,P2),rdf(S,P1,O)],
  rdf(S,P2,O),
  G
):-
  rdf(P1, rdfs:subPropertyOf, P2, G),
  rdf_iri(P1),
  rdf_iri(P2),
  rdf(S, P1, O, G).


% [rdfs8] Classes are instances of =|rdfs:Resource|=.

rdf:explanation(rdfs, rdfs8, 'Classes are instances of rdfs:Resource.').

rdf:rule(
  rdfs,
  rdfs8,
  [rdf(C,rdf:type,rdfs:'Class')],
  rdf(C,rdfs:subClassOf,rdfs:'Resource'),
  G
):-
  rdf(C, rdf:type, rdfs:'Class', G).


% [rdfs9] Using the class hierarchy.

rdf:explanation(rdfs, rdfs9, 'Using the class hierarchy.').

rdf:rule(
  rdfs,
  rdfs9,
  [rdf(C1,rdfs:subClassOf,C2),rdf(I,rdf:type,C1)],
  rdf(I,rdf:type,C2),
  G
):-
  rdf(C1, rdfs:subClassOf, C2, G),
  % `C1` is automatically constrained to blank nodes and IRIs,
  % since is must have appeared as the subject term of some triple.
  rdf(I, rdf:type, C1, G).


% [rdfs10] Reflexivity of the class hierarchy relation.

rdf:explanation(rdfs, rdfs10, 'Reflexivity of the class hierarchy relation.').

rdf:rule(
  rdfs,
  rdfs10,
  [rdf(C,rdf:type,rdfs:'Class')],
  rdf(C,rdfs:subClassOf,C),
  G
):-
  rdf(C, rdf:type, rdfs:'Class', G).


% [rdfs11] Transitivity of the class hierarchy relation.

rdf:explanation(
  rdfs,
  rdfs11,
  'Transitivity of the class hierarchy relation.'
).

rdf:rule(
  rdfs,
  rdfs11,
  [rdf(C1,rdfs:subClassOf,C2),rdf(C2,rdfs:subClassOf,C3)],
  rdf(C1,rdfs:subClassOf,C3),
  G
):-
  rdf(C1, rdfs:subClassOf, C2, G),
  \+ rdf_is_literal(C2),
  rdf(C2, rdfs:subClassOf, C3, G).


% [rdfs12] Container membership properties are subproperties
%          of the member property.

rdf:explanation(
  rdfs,
  rdfs12,
  'Container membership properties are subproperties of the member property.'
).

rdf:rule(
  rdfs,
  rdfs12,
  [rdf(P,rdf:type,rdfs:'ContainerMembershipProperty')],
  rdf(P,rdfs:subPropertyOf,rdfs:member),
  G
):-
  rdf(P, rdf:type, rdfs:'ContainerMembershipProperty', G).


% [rdfs13] Datatypes are subclasses of literal.

rdf:explanation(rdfs, rdfs13, 'Datatypes are subclasses of literal.').

rdf:rule(
  rdfs,
  rdfs13,
  [rdf(D,rdf:type,rdfs:'Datatype')],
  rdf(D,rdfs:subClassOf,rdfs:'Literal'),
  G
):-
  rdf(D, rdf:type, rdfs:'Datatype', G).



% RDFS axiomatic triples: domain.
rdf:axiom(rdfs, rdf( rdf:type,         rdfs:domain,rdfs:'Resource' )).
rdf:axiom(rdfs, rdf(rdfs:domain,       rdfs:domain, rdf:'Property' )).
rdf:axiom(rdfs, rdf(rdfs:range,        rdfs:domain, rdf:'Property' )).
rdf:axiom(rdfs, rdf(rdfs:subPropertyOf,rdfs:domain, rdf:'Property' )).
rdf:axiom(rdfs, rdf(rdfs:subClassOf,   rdfs:domain,rdfs:'Class'    )).
rdf:axiom(rdfs, rdf( rdf:subject,      rdfs:domain, rdf:'Statement')).
rdf:axiom(rdfs, rdf( rdf:predicate,    rdfs:domain, rdf:'Statement')).
rdf:axiom(rdfs, rdf( rdf:object,       rdfs:domain, rdf:'Statement')).
rdf:axiom(rdfs, rdf(rdfs:member,       rdfs:domain,rdfs:'Resource' )).
rdf:axiom(rdfs, rdf( rdf:first,        rdfs:domain, rdf:'List'     )).
rdf:axiom(rdfs, rdf( rdf:rest,         rdfs:domain, rdf:'List'     )).
rdf:axiom(rdfs, rdf(rdfs:seeAlso,      rdfs:domain,rdfs:'Resource' )).
rdf:axiom(rdfs, rdf(rdfs:isDefinedBy,  rdfs:domain,rdfs:'Resource' )).
rdf:axiom(rdfs, rdf(rdfs:comment,      rdfs:domain,rdfs:'Resource' )).
rdf:axiom(rdfs, rdf(rdfs:label,        rdfs:domain,rdfs:'Resource' )).
rdf:axiom(rdfs, rdf( rdf:value,        rdfs:domain,rdfs:'Resource' )).

% RDFS axiomatic triples: range.
rdf:axiom(rdfs, rdf( rdf:type,         rdfs:range,rdfs:'Class'   )).
rdf:axiom(rdfs, rdf(rdfs:domain,       rdfs:range,rdfs:'Class'   )).
rdf:axiom(rdfs, rdf(rdfs:range,        rdfs:range,rdfs:'Class'   )).
rdf:axiom(rdfs, rdf(rdfs:subPropertyOf,rdfs:range, rdf:'Property')).
rdf:axiom(rdfs, rdf(rdfs:subClassOf,   rdfs:range,rdfs:'Class'   )).
rdf:axiom(rdfs, rdf( rdf:subject,      rdfs:range,rdfs:'Resource')).
rdf:axiom(rdfs, rdf( rdf:predicate,    rdfs:range,rdfs:'Resource')).
rdf:axiom(rdfs, rdf( rdf:object,       rdfs:range,rdfs:'Resource')).
rdf:axiom(rdfs, rdf(rdfs:member,       rdfs:range,rdfs:'Resource')).
rdf:axiom(rdfs, rdf( rdf:first,        rdfs:range,rdfs:'Resource')).
rdf:axiom(rdfs, rdf( rdf:rest,         rdfs:range, rdf:'List'    )).
rdf:axiom(rdfs, rdf(rdfs:seeAlso,      rdfs:range,rdfs:'Resource')).
rdf:axiom(rdfs, rdf(rdfs:isDefinedBy,  rdfs:range,rdfs:'Resource')).
rdf:axiom(rdfs, rdf(rdfs:comment,      rdfs:range,rdfs:'Literal' )).
rdf:axiom(rdfs, rdf(rdfs:label,        rdfs:range,rdfs:'Literal' )).
rdf:axiom(rdfs, rdf( rdf:value,        rdfs:range,rdfs:'Resource')).

% RDFS axiomatic triples: subclass hierarchy.
rdf:axiom(rdfs, rdf( rdf:'Alt',rdfs:subClassOf,rdfs:'Container')).
rdf:axiom(rdfs, rdf( rdf:'Bag',rdfs:subClassOf,rdfs:'Container')).
rdf:axiom(rdfs, rdf( rdf:'Seq',rdfs:subClassOf,rdfs:'Container')).
rdf:axiom(
  rdfs,
  rdf(rdfs:'ContainerMembershipProperty',rdfs:subClassOf,rdf:'Property')
).

% RDFS axiomatic triples: subproperty hierarchy.
rdf:axiom(rdfs, rdf(rdfs:isDefinedBy,rdfs:subPropertyOf,rdfs:seeAlso)).

% RDFS axiomatic triples: datatypes.
rdf:axiom(rdfs, rdf( rdf:'XMLLiteral', rdf:type,      rdfs:'Datatype')).
rdf:axiom(rdfs, rdf( rdf:'XMLLiteral',rdfs:subClassOf,rdfs:'Literal' )).
rdf:axiom(rdfs, rdf(rdfs:'Datatype',  rdfs:subClassOf,rdfs:'Class'   )).

% RDFS axiomatic triples: container membership properies.
% There is an infinite number of RDFS axioms for integer enumeration.
rdf:axiom(rdfs, rdf(P,rdf:type,rdfs:'ContainerMembershipProperty')):-
  between(1, _, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, P).
rdf:axiom(rdfs, rdf(P,rdfs:domain,rdfs:'Resource')):-
  between(1, _, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, P).
rdf:axiom(rdfs, rdf(P,rdfs:range,rdfs:'Resource')):-
  between(1, _, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, P).

