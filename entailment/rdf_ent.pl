:- module(
  rdf_ent,
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

/** <module> RDF entailment

Specification of entailment rules for RDF.

@author Wouter Beek
@see rdf-mt 1.1 (2014)
@tbd Can prefix expansion be fixed?
@version 2013/08-2013/09, 2014/07
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(settings)).

:- use_module(math(math_ext)).

:- use_module(plRdf(entailment/rdf_bnode_map)).

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

:- setting(
     rdf:max_enumerator,
     any,
     inf,
     'The maximum enumerator property that is considered.'
).



rdf:regime(se).


% [se1] Existential quantification w.r.t. the object term.

rdf:explanation(
  se,
  se1,
  'Existential quantification w.r.t. the object term.'
).

rdf:rule_forward(se, se1, [rdf(S,P,O)], rdf(S,P,B), G):-
  rdf(S, P, O, G),

  %%%%% THIS RESTRICTS THE STANDARD.
  %%%%\+ rdf_is_bnode(O),

  % Use an existing mapping, if it exists.
  % Add a new mapping, otherwise.
  term_set_bnode(G, O, B).


% [se2] Existential quantification w.r.t. the subject term.

rdf:explanation(
  se,
  se2,
  'Existential quantification w.r.t. the subject term.'
).

rdf:rule_forward(se, se2, [rdf(S,P,O)], rdf(B,P,O), G):-
  rdf(S, P, O, G),

  %%%%% THIS RESTRICTS THE STANDARD.
  %%%%\+ rdf_is_bnode(S),

  % Use an existing mapping, if it exists.
  % Add a new mapping, otherwise.
  term_set_bnode(G, S, B).


% [lg] Literal generalization is a special case of [se1],
%      where the object term is a literal.
%      Literal generalization is used whenever something has to be
%      predicated of a literal (since literals cannot occur
%      as subject terms).

rdf:explanation(
  rdf,
  lg,
  'Literal generalization is a special case of [se1],\c
   where the object term is a literal.\c
   Literal generalization is used whenever something has to be predicated of\c
   a literal (since literals cannot occur as subject terms).'
).

rdf:rule_forward(rdf, lg, [rdf(S,P,Lit)], rdf(S,P,B), G):-
  rdf(S, P, Lit, G),
  rdf_is_literal(Lit),
  term_set_bnode(G, Lit, B).



rdf:regime(rdf).


% [rdf1] Predicate terms are instances of =|rdf:'Property'`.

rdf:explanation(
  rdf,
  rdf1,
  'Terms that occur in the predicate position are instances of rdf:Property.'
).

rdf:rule_forward(
  rdf,
  rdf1,
  [rdf(S,P,O)],
  rdf(P,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'),
  G
):-
  rdf(S, P, O, G).


% [rdf2] XML literals are instances of =|rdf:'XMLLiteral'`.

rdf:explanation(
  rdf,
  rdf2,
  'XML literals are instances of rdf:XMLLiteral.'
).

rdf:rule_forward(
  rdf,
  rdf2,
  [rdf(S,P,TypedLit)],
  rdf(B,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral'),
  G
):-
  rdf(S, P, TypedLit, G),

  % @tbd This should be a well-typed XML literal...
  rdf_is_literal(TypedLit),

  term_set_bnode(G, TypedLit, B).


% RDF axiomatic triples.
rdf:axiom(
  rdf,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdf,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#subject',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdf,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdf,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#object',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdf,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdf,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
rdf:axiom(
  rdf,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#value',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
).
% There is an infinite number of integer enumerator axioms.
rdf:axiom(
  rdf,
  rdf(
    P,
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
  )
):-
  setting(rdf:max_enumerator, High),
  betwixt(1, High, I),
  format(atom(Name), '_~w', [I]),
  rdf_global_id(rdf:Name, P).
rdf:axiom(
  rdf,
  rdf(
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'
  )
).

