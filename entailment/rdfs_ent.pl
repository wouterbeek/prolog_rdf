:- module(
  rdfs_ent,
  [
    rdf:axiom/2, % ?Regime:atom
                 % ?Axiom:compound
    rdf:explanation/3, % ?Regime:atom
                       % ?Rule:atom
                       % ?Explanation:atom
    rdf:rule_forward/5 % ?Regime:atom
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
@tbd Can prefix expansion be fixed?
@version 2013/08-2013/09, 2014/06-2014/07
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(settings)).

:- use_module(math(math_ext)).

:- use_module(plRdf(entailment/rdf_bnode_map)).
:- use_module(plRdf(term/rdf_literal)).
:- use_module(plRdf(term/rdf_term)).

%! rdf:axiom(?Regime:atom, ?Axiom:compound) is nondet.

:- discontiguous(rdf:axiom/2).
:- multifile(rdf:axiom/2).
:- rdf_meta(rdf:axiom(?,t)).

%! rdf:explanation(?Regime:atom, ?Rule:atom, ?Explanation:atom) is nondet.

:- discontiguous(rdf:explanation/3).
:- multifile(rdf:explanation/3).

%! rdf:rule_forward(
%!   ?Regime:atom,
%!   ?Rule:atom,
%!   ?Premises:list(compound),
%!   ?Conclusion:compound,
%!   ?Graph:atom
%! ) is nondet.

:- discontiguous(rdf:rule_forward/5).
:- multifile(rdf:rule_forward/5).
:- rdf_meta(rdf:rule_forward(?,?,t,t,?)).

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

rdf:rule_forward(rdfs, gl, [rdf(S,P,B)], rdf(S,P,Lit), G):-
  rdf(S, P, B, G),
  % If the object term is not a blank node,
  % then we do not have to search the blank node-literal mapping.
  rdf_is_bnode(B),

  % Not every blank node that is an object term in some triple
  % is a generalization for a literal.
  % Therefore, it has to occur in the mapping established by rule [lg].
  bnode_to_term(G, B, Lit),
  rdf_is_literal(Lit).


% [rdfs1] Literals are instances of
%         `http://www.w3.org/2000/01/rdf-schema#Literal`.

rdf:explanation(
  rdfs,
  rdfs1,
  'Literal terms belong to the extension of the RDFS literal class.'
).

rdf:rule_forward(
  rdfs,
  rdfs1,
  [rdf(S,P,PlainLit)],
  rdf(
    B,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#Literal'
  ),
  G
):-
  rdf(S, P, PlainLit, G),
  rdf_plain_literal_term(PlainLit),

  term_set_bnode(G, PlainLit, B).


% [rdfs2] Class membership through domain restriction.

rdf:explanation(rdfs, rdfs2, 'Class membership through domain restriction.').

rdf:rule_forward(
  rdfs,
  rdfs2,
  [rdf(P,'http://www.w3.org/2000/01/rdf-schema#domain',C),rdf(S,P,O)],
  rdf(S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C),
  G
):-
  rdf(P, 'http://www.w3.org/2000/01/rdf-schema#domain', C, G),
  rdf_iri(P),
  rdf(S, P, O, G).


% [rdfs3] Class membership through range restriction.

rdf:explanation(rdfs, rdfs3, 'Class membership through range restriction.').

rdf:rule_forward(
  rdfs,
  rdfs3,
  [rdf(P,'http://www.w3.org/2000/01/rdf-schema#range',C),rdf(S,P,O)],
  rdf(O,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C),
  G
):-
  rdf(P, 'http://www.w3.org/2000/01/rdf-schema#range', C, G),
  rdf_iri(P),
  rdf(S, P, O, G),
  \+ rdf_is_literal(O).


% [rdfs4a] Subject terms are resources.

rdf:explanation(
  rdfs,
  rdfs4a,
  'Terms that occur in the subject position are RDFS resources.'
).

rdf:rule_forward(
  rdfs,
  rdfs4a,
  [rdf(S,P,O)],
  rdf(
    S,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  ),
  G
):-
  rdf(S, P, O, G).


% [rdfs4b] Object terms are resources.

rdf:explanation(
  rdfs,
  rdfs4b,
  'Terms that occur in the object position are RDFS resources.'
).

rdf:rule_forward(
  rdfs,
  rdfs4b,
  [rdf(S,P,O)],
  rdf(
    O,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  ),
  G
):-
  rdf(S, P, O, G),
  \+ rdf_is_literal(O).


% [rdfs5] Transitive closure of the property hierarchy relation.

rdf:explanation(
  rdfs,
  rdfs5,
  'Transitive closure of the property hierarchy relation.'
).

rdf:rule_forward(
  rdfs,
  rdfs5,
  [
    rdf(P1,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',P2),
    rdf(P2,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',P3)
  ],
  rdf(P1,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',P3),
  G
):-
  rdf(P1, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', P2, G),
  % `P2` is automatically constrained to blank nodes and IRIs,
  % since is must appear as the subject term of some triple.
  rdf(P2, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', P3, G).


% [rdfs6] Reflexivity of the property hierarchy relation.

rdf:explanation(
  rdfs,
  rdfs6,
  'Reflexivity of the property hierarchy relation.'
).

rdf:rule_forward(
  rdfs,
  rdfs6,
  [
    rdf(
      P,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
    )
  ],
  rdf(P,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',P),
  G
):-
  rdf(
    P,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property',
    G
  ).


% [rdfs7] Using the property hierarchy.

rdf:explanation(rdfs, rdfs7, 'Using the property hierarchy.').

rdf:rule_forward(
  rdfs,
  rdfs7,
  [
    rdf(P1,'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',P2),
    rdf(S,P1,O)
  ],
  rdf(S,P2,O),
  G
):-
  rdf(P1, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', P2, G),
  rdf_iri(P1),
  rdf_iri(P2),
  rdf(S, P1, O, G).


% [rdfs8] Classes are instances of =|'http://www.w3.org/2000/01/rdf-schema#Resource'`.

rdf:explanation(rdfs, rdfs8, 'Classes are instances of =|rdfs:Resource`').

rdf:rule_forward(
  rdfs,
  rdfs8,
  [
    rdf(
      C,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      'http://www.w3.org/2000/01/rdf-schema#Class'
    )
  ],
  rdf(
    C,
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  ),
  G
):-
  rdf(
    C,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#Class',
    G
  ).


% [rdfs9] Using the class hierarchy.

rdf:explanation(
  rdfs,
  rdfs9,
  'Instantiation is closed under the class hierarchy.'
).

rdf:rule_forward(
  rdfs,
  rdfs9,
  [
    rdf(C1,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C2),
    rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C1)
  ],
  rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C2),
  G
):-
  rdf(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2, G),
  % `C1` is automatically constrained to blank nodes and IRIs,
  % since is must have appeared as the subject term of some triple.
  rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1, G).


% [rdfs10] Reflexivity of the class hierarchy relation.

rdf:explanation(rdfs, rdfs10, 'Reflexivity of the class hierarchy relation.').

rdf:rule_forward(
  rdfs,
  rdfs10,
  [
    rdf(
      C,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      'http://www.w3.org/2000/01/rdf-schema#Class'
    )
  ],
  rdf(C,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C),
  G
):-
  rdf(
    C,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#Class',
    G
  ).


% [rdfs11] Transitivity of the class hierarchy relation.

rdf:explanation(
  rdfs,
  rdfs11,
  'Transitivity of the class hierarchy relation.'
).

rdf:rule_forward(
  rdfs,
  rdfs11,
  [
    rdf(C1,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C2),
    rdf(C2,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C3)
  ],
  rdf(C1,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C3),
  G
):-
  rdf(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2, G),
  \+ rdf_is_literal(C2),
  rdf(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C3, G).


% [rdfs12] Container membership properties are subproperties
%          of the member property.

rdf:explanation(
  rdfs,
  rdfs12,
  'Container membership properties are subproperties of the member property.'
).

rdf:rule_forward(
  rdfs,
  rdfs12,
  [
    rdf(
      P,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      'http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty'
    )
  ],
  rdf(
    P,
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',
    'http://www.w3.org/2000/01/rdf-schema#member'
  ),
  G
):-
  rdf(
    P,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty',
    G
  ).


% [rdfs13] Datatypes are subclasses of literal.

rdf:explanation(rdfs, rdfs13, 'Datatypes are subclasses of literal.').

rdf:rule_forward(
  rdfs,
  rdfs13,
  [
    rdf(
      D,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      'http://www.w3.org/2000/01/rdf-schema#Datatype'
    )
  ],
  rdf(
    D,
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/2000/01/rdf-schema#Literal'
  ),
  G
):-
  rdf(
    D,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#Datatype',
    G
  ).



% RDFS axiomatic triples: domain.
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#Class'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#subject',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#object',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#member',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#seeAlso',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#isDefinedBy',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#comment',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#label',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#value',
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).

% RDFS axiomatic triples: range.
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Class'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Class'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Class'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Class'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#subject',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#object',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#member',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#seeAlso',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#isDefinedBy',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#comment',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Literal'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#label',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Literal'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#value',
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
).

% RDFS axiomatic triples: subclass hierarchy.
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Alt',
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/2000/01/rdf-schema#Container'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag',
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/2000/01/rdf-schema#Container'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq',
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/2000/01/rdf-schema#Container'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty',
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).

% RDFS axiomatic triples: subproperty hierarchy.
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#isDefinedBy',
    'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',
    'http://www.w3.org/2000/01/rdf-schema#seeAlso'
  )
).

% RDFS axiomatic triples: datatypes.
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#Datatype'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral',
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/2000/01/rdf-schema#Literal'
  )
).
rdf:axiom(
  rdfs,
  rdf(
    'http://www.w3.org/2000/01/rdf-schema#Datatype',
    'http://www.w3.org/2000/01/rdf-schema#subClassOf',
    'http://www.w3.org/2000/01/rdf-schema#Class'
  )
).

% RDFS axiomatic triples: container membership properies.
% There is an infinite number of RDFS axioms for integer enumeration.
rdf:axiom(
  rdfs,
  rdf(
    P,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty'
  )
):-
  setting(rdf:max_enumerator, High),
  betwixt(1, High, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, P).
rdf:axiom(
  rdfs,
  rdf(
    P,
    'http://www.w3.org/2000/01/rdf-schema#domain',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
):-
  setting(rdf:max_enumerator, High),
  betwixt(1, High, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, P).
rdf:axiom(
  rdfs,
  rdf(
    P,
    'http://www.w3.org/2000/01/rdf-schema#range',
    'http://www.w3.org/2000/01/rdf-schema#Resource'
  )
):-
  setting(rdf:max_enumerator, High),
  betwixt(1, High, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, P).

