:- module(
  rdf_ent,
  [
    axiom/4, % ?Regime:atom
             % ?Subject:or([bnode,iri])
             % ?Predicate:iri
             % ?Object:or([bnode,literal,iri])
    explanation/3, % ?Regime:atom
                   % ?Rule:atom
                   % ?Explanation:atom
    rule/7 % ?Regime:atom
           % ?Rule:atom
           % ?Premises:list(compound)
           % ?Subject:or([bnode,iri])
           % ?Predicate:iri
           % ?Object:or([bnode,iri,literal])
           % ?Graph:atom
  ]
).

/** <module> RDF materialization

Axioms and rules for RDF materialization, as defined by Hayes2004.

@author Wouter Beek
@see Hayes2004
@tbd Add the check for well-typed XML literals to rule `rdf2`.
@version 2013/08-2013/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_reasoning(rdf_bnode_map)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

%! axiom(
%!   ?Regime:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

:- discontiguous(axiom/4).
:- rdf_meta(axiom(?,r,r,r)).

%! explanation(?Regime:atom, ?Rule:atom, ?Explanation:atom) is nondet.

:- discontiguous(explanation/3).
:- rdf_meta(explanation(?,?,?)).

%! rule(
%!   ?Regime:atom,
%!   ?Rule:atom,
%!   ?Premises:list(compound),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?Graph:atom
%! ) is nondet.

:- discontiguous(rule/7).
:- rdf_meta(rule(?,?,?,r,r,r,?)).

:- discontiguous(user:regime/1).
:- multifile(user:regime/1).



user:regime(se).

% [se1] Existential quantification w.r.t. the object term.
rule(se, se1, [rdf(S,P,O)], S, P, B, G):-
  rdf(S, P, O, G),
  % THIS RESTRICTS THE STANDARD.
  \+ rdf_is_bnode(O),
  r2b(G, O, B).

% [se2] Existential quantification w.r.t. the subject term.
rule(se, se2, [rdf(S,P,O)], B, P, O, G):-
  rdf(S, P, O, G),
  % THIS RESTRICTS THE STANDARD.
  \+ rdf_is_bnode(S),
  r2b(G, S, B).

/*
% [lg] Literal generalization is a special case of [se1],
%      where the object term is a literal.
%      Literal generalization is used whenever something has to be
%      predicated of a literal (since literals cannot occur
%      as subject terms).
rule(rdf, lg, [rdf(S,P,Lit)], S, P, B, G):-
  rdf(S, P, Lit, G),
  rdf_is_literal(Lit),
  r2b(G, O, B).
*/


user:regime(rdf).

% [rdf1] Predicate terms are instances of =|rdf:'Property'|=.
explanation(
  rdf,
  rdf1,
  'Terms that occur in the predicate position are instances of rdf:Property.'
).
rule(rdf, rdf1, [rdf(S,P,O)], P, rdf:type, rdf:'Property', G):-
  rdf(S, P, O, G).

% [rdf2] XML literals are instances of =|rdf:'XMLLiteral'|=.
explanation(rdf, rdf2, 'All XML literals are instances of rdf:XMLLiteral.').
rule(rdf, rdf2, [rdf(S,P,TypedLit)], B, rdf:type, rdf:'XMLLiteral', G):-
  rdf(S, P, TypedLit, G),
  % @tbd This should be a well-typed XML literal...
  rdf_is_literal(TypedLit),
  r2b(G, TypedLit, B).

% RDF axiomatic triples.
axiom(rdf, rdf:type,      rdf:type, rdf:'Property').
axiom(rdf, rdf:subject,   rdf:type, rdf:'Property').
axiom(rdf, rdf:predicate, rdf:type, rdf:'Property').
axiom(rdf, rdf:object,    rdf:type, rdf:'Property').
axiom(rdf, rdf:first,     rdf:type, rdf:'Property').
axiom(rdf, rdf:rest,      rdf:type, rdf:'Property').
axiom(rdf, rdf:value,     rdf:type, rdf:'Property').
% There is an infinite number of integer enumerator axioms...
axiom(rdf, IRI, rdf:type, rdf:'Property'):-
  between(1, 3, I),
  format(atom(Name), '_~w', [I]),
  rdf_global_id(rdf:Name, IRI).
axiom(rdf, rdf:nil,       rdf:type, rdf:'List'    ).

