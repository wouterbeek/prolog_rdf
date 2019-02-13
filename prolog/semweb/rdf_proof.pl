:- module(
  rdf_proof,
  [
    rdf_export_proof/2, % +File, +Proof
    rdf_view_proof/1    % +Proof
  ]
).

/** <module> Support predicates for RDF proofs

@author Wouter Beek
@version 2019
*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(graph/gv)).
:- use_module(library(graph/dot)).
:- use_module(library(semweb/rdf_print)).





%! rdf_export_proof(+File:atom, +Proof:compound) is det.

rdf_export_proof(File, Proof) :-
  gv_export(File, {Proof}/[Out]>>export_proof(Out, Proof), [directed(true)]).



%! rdf_view_proof(+Proof:compound) is det.

rdf_view_proof(Proof) :-
  gv_view({Proof}/[Out]>>export_proof(Out, Proof), [directed(true)]).





% GENERICS %

%! export_proof(+Out:stream, +Proof:compound) is det

export_proof(Out, Proof) :-
  Proof = t(Rule,Concl,Subproofs),
  % conclusion node
  dcg_with_output_to(string(ConclLabel), rdf_dcg_tp(Concl)),
  dot_node(Out, Concl, [label(ConclLabel)]),
  % rule application node
  format(string(RuleLabel), "[~w]", [Rule]),
  dot_node(Out, Proof, [label(RuleLabel)]),
  % arc from conclusion to rule application node
  dot_arc(Out, Concl, Proof),
  maplist(export_subproof(Out, Proof), Subproofs).

export_subproof(Out, Proof, Subproof) :-
  Subproof = t(_,Prem,_),
  dcg_with_output_to(string(PremLabel), rdf_dcg_tp(Prem)),
  dot_node(Out, Prem, [label(PremLabel)]),
  dot_arc(Out, Proof, Prem),
  export_proof(Out, Subproof).
