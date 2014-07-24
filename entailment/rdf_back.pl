:- module(
  rdf_back,
  [
    rdf_back/2 % +Graph:atom
               % ?Triple:compound
  ]
).

/** <module> RDF back

Backward chaining RDF(S) reasoning.

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(meta_ext)).

:- use_module(plRdf_ent(rdf_ent)). % Axioms, explanations, rules.
:- use_module(plRdf_ent(rdfs_ent)). % Axioms, explanations, rules.

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

:- rdf_meta(rdf_back(?,t)).



% No reasoning whatsoever.
rdf_back(Graph, rdf(S,P,O)):-
  Regimes = [rdf],
  default(user, Graph),
  
  % NONDET
  member(Regime, Regimes),
  % NONDET
gtrace,
  rdf:rule(Regime, _Rule, Premises, rdf(S0,P0,O0), Graph),
  % @tbd Can prefix expansion be fixed?
  maplist(rdf_global_id, [S0,P0,O0], [S,P,O]),

  forall(
    member(Premise, Premises),
    rdf_back(Graph, Premise)
  ).



% Helpers

% All axioms can be deduced.
rdf:rule(Regime, axiom, [], Axiom, _):-
  rdf:axiom(Regime, Axiom).
% All facts can be deduced.
rdf:rule(Regime, fact, [], rdf(S,P,O), Graph):-
  rdf(S, P, O, Graph).

