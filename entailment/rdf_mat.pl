:- module(
  rdf_mat,
  [
    materialize/2 % +Graph:atom
                  % +Options:list(nvpair)
  ]
).

/** <module> RDF materialization engine

Takes axioms, rules, and the RDF index and performs materializations.

@author Wouter Beek
@version 2013/09-2013/10, 2013/12-2014/01, 2014/06-2014/07
*/

:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(predicate_options)). % Declarations.
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_collection)). % DCG-meta.
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(doyle(doyle)).
:- use_module(generics(deb_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(thread_ext)).
:- use_module(tms(tms)).
:- use_module(tms(tms_print)).

:- use_module(plRdf_ent(rdf_ent)). % Axioms, explanations, rules.
:- use_module(plRdf_ent(rdfs_ent)). % Axioms, explanations, rules.

%! recent_triple(?Graph:atom, ?Triple:compound) is semidet.

:- dynamic(recent_triple/2).

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
   % All axioms can be deduced as if they are the outcome of a rule.
   rdf:rule(Regime, axiom, [], Axiom, _):-
     rdf:axiom(Regime, Axiom).

%! rdf:regime(?Regime:atom) is nondet.

:- discontiguous(rdf:regime/1).
:- multifile(rdf:regime/1).

:- predicate_options(materialize/2, 2, [
     entailment_regimes(+list(atom)),
     pass_to(materialize/3, 3)
   ]).
:- predicate_options(materialize/3, 3, [
     entailment_regimes(+list(atom)),
     multiple_justifications(+boolean)
   ]).



%! materialize(?Graph:atom, +Options:list(nvpair)) is det.
% Performs all depth-one deductions for either the given RDF graph
% or no RDF graph.
%
% If the graph parameter is given, then only triples from that graph
% are considered for materialization.
% The same graph is used for storing the results.
%
% The following options are supported:
%   * =|entailment_regimes(+list(atom))|=
%     Default: `[rdf,rdfs]`
%   * =|multiple_justifications(+boolean)|=
%     Whether a single proposition can have more than one justification.

% No materialization whatsoever.
materialize(_, Options):-
  option(entailment_regimes(Regimes), Options),
  memberchk(none, Regimes), !.
% Some form of materialization.
materialize(Graph, Options):-
  % The default graph is called `user`.
  % This is also the default graph that rdf/3 writes to.
  default(user, Graph),

  % Make sure there is an existing/registered TMS that can be used.
  % Otherwise, we have to create a new TMS for this purpose.
  % We choose a TMS according to Doyle's orginal specification (classic!).
  graph_tms(Graph, Tms),

  % DEB
  if_debug(rdf_mat, retractall(recent_triple(_,_))),

  materialize(Tms, Graph, Options).

%! materialize(+TMS:atom, +Graph:atom, +Options:list(nvpair)) is det.
% The inner loop of materialization.
% This performs all depth-1 reasoning steps (i.e., breadth-first).
%
% ### Regime specification
%
% We cannot always assume that a 'lower' regime is applicable,
% when a 'higher' regime is requested.
% For instance with `[rdf,rdfs]` we do **not** intend to use `se`.
%
% ### Options
%
% The following options are supported:
%   * `entailment_regimes(?EntailmentRegimes:list(oneof([se,rdf,rdfs])))`
%   * `multiple_justifications(?MultipleJustifications:boolean)`
%     Keep adding new justifications for old nodes.
%
% ### Arguments
%
% @arg Regimes An ordered set of atomic names denoting
%      the entailment regimes that are used for materialization.
% @arg TMS The atomic name of a Truth Maintenance System.
% @arg Graph The atomic name of a graph.

materialize(Tms, Graph, Options):-
  % A deduction of depth one.
  option(entailment_regimes(Regimes), Options, [rdf,rdfs]),
  member(Regime, Regimes),
  rdf:rule(Regime, Rule, Premises, rdf(S,P,O), Graph),
  
  % Only accept new justifications.
  % A proposition may have multiple justifications.
  (
    option(multiple_justifications(true), Options, false)
  ->
    \+ tms_justification(Tms, Premises, Rule, rdf(S,P,O))
  ;
    \+ rdf(S, P, O, Graph)
  ),
  
  % Add to TMS.
  doyle_add_argument(Tms, Premises, Rule, rdf(S,P,O), Justification),

  % DEB
  if_debug(
    rdf_mat,
    (
      assert(recent_triple(Graph, rdf(S,P,O))),
      dcg_with_output_to(atom(Msg), materialize_message(Tms, Justification)),
      debug(rdf_mat, '~a', [Msg])
    )
  ),
  
  % Store the result.
  rdf_assert(S, P, O, Graph), !,
  
  % Look for more results.
  materialize(Tms, Graph, Options).
% Done!
materialize(_, _, Options):-
  % DEB
  option(entailment_regimes(Regimes), Options, [rdf,rdfs]),
  dcg_with_output_to(atom(RegimesAtom), list(pl_term, Regimes)),
  if_debug(
    rdf_mat,
    (
      flag(deductions, N, 0),
      debug(rdf_mat, 'Added ~w deductions (regimes: ~w).', [N,RegimesAtom])
    )
  ).



% Helpers

%! graph_tms(+Graph:atom, -Tms:atom) is det.
% Returns either the existing TMS for the given graph,
% or creates and returns a new one.

graph_tms(Graph, Tms):-
  atomic_list_concat([tms,Graph], '_', Tms),
  ensure_tms(Tms).

ensure_tms(Tms):-
  tms(Tms), !.
ensure_tms(Tms):-
  register_tms(doyle, Tms),
  doyle_reset(Tms),
  doyle_init(Tms).


%! materialize_message(+Tms:atom, +Justification:compound)// is det.

materialize_message(Tms, Justification) -->
  {flag(deductions, Id, Id + 1)},
  integer(Id),
  `: `,
  tms_print_justification([indent(0),lang(en)], Tms, Justification),
  nl.

