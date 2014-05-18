:- module(
  rdf_mat,
  [
    materialize/2, % +Options:list(nvpair)
                   % +Graph:atom
    regime/1, % ?Regime:oneof([none,rdf,rdfs,se])
    start_materializer/3 % +Options:list(nvpair)
                         % +Interval:positive_integer
                         % +Graph:atom
  ]
).

/** <module> RDF materialization engine

Takes axioms, rules, and the RDF index and performs materializations.

@author Wouter Beek
@version 2013/09-2013/10, 2013/12-2014/01
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_collection)). % DCG-meta.
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(doyle(doyle)).
:- use_module(generics(deb_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(
  rdf_reasoning(rdf_ent),
  [
    axiom/4 as rdf_axiom,
    explanation/3 as rdf_explanation,
    rule/7 as rdf_rule
  ]
).
:- use_module(
  rdfs(rdfs_ent),
  [
    axiom/4 as rdfs_axiom,
    explanation/3 as rdfs_explanation,
    rule/7 as rdfs_rule
  ]
).
:- use_module(tms(tms)).
:- use_module(tms(tms_print)).

:- dynamic(recent_triple/4).



%! axiom(
%!   ?Regime:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

axiom(Regime, S, P, O):-
  rdf_axiom(Regime, S, P, O).
axiom(Regime, S, P, O):-
  rdfs_axiom(Regime, S, P, O).

explanation(Regime, Rule, Explanation):-
  rdf_explanation(Regime, Rule, Explanation).
explanation(Regime, Rule, Explanation):-
  rdfs_explanation(Regime, Rule, Explanation).

%! materialize(+Options:list(nvpair), ?Graph:atom) is det.
% Performs all depth-one deductions for either the given RDF graph
% or no RDF graph.
%
% If the graph parameter is given, then only triples from that graph
% are considered for materialization.
% The same graph is used for storing the results.
%
% The following options are supported:
%   * =|entailment_regimes(Regimes:list(oneof([none,rdf,rdfs])))|=
%     Default: `[rdf,rdfs]`
%   * =|multiple_justifications(Multiple:boolean)|=
%     Whether a single proposition can have more than one justification.
%
% @arg Regimes An ordered set of atomic names denoting
%      the entailment regimes that are used by materialization.
% @arg Graph Either the atomic name of a graph
%      or uninstantiated (not restricted to a particular graph).

% No materialization whatsoever.
materialize(O1, _G):-
  option(entailment_regimes(ERs), O1),
  memberchk(none, ERs), !.
% Some form of materialization.
materialize(O1, G):-
  % The default graph is called `user`.
  % This is also the default graph that rdf/3 write to.
  (nonvar(G), ! ; G = user),

  % Make sure there is a registered TMS that can be used.
  % Otherwise, we have to create a TMS for this purpose.
  % We choose a TMS according to Doyle's orginal specification (classic!).
  atom_concat(tms_, G, TMS),
  (
    tms(TMS), !
  ;
    register_tms(doyle, TMS),
    doyle_reset(TMS),
    doyle_init(TMS)
  ),

  % DEB
  if_debug(rdf_mat, retractall(recent_triple(_,_,_,_))),

  % Let's go!
  materialize(O1, TMS, G).

%! materialize(+Options:list(nvpair), +TMS:atom, +Graph:atom) is det.
% The inner loop of materialization.
% This performs all depth-1 reasoning steps (i.e., breadth-first).
%
% # Regime specification
%
% We cannot always assume that a 'lower' regime is applicable,
%  when a 'higher' regime is requested.
% For instance with `[rdf,rdfs]` we do **not** intend to use `se`.
%
% # Options
%
% The following options are supported:
%   * `entailment_regimes(?EntailmentRegimes:list(oneof([se,rdf,rdfs])))`
%   * `multiple_justifications(?MultipleJustifications:boolean)`
%     Keep adding new justifications for old nodes.
%
% @arg Regimes An ordered set of atomic names denoting
%        the entailment regimes that are used for materialization.
% @arg TMS The atomic name of a Truth Maintenance System.
% @arg Graph The atomic name of a graph.

materialize(O1, TMS, G):-
  % A deduction of depth one.
  option(entailment_regimes(ERs), O1, [rdf,rdfs]),
  member(ER, ERs),
  rule(ER, Rule, Premises, S, P, O, G),
  
  % Only accept new justifications
  % (one proposition may have multiple justifications).
  (
    option(multiple_justifications(true), O1, false)
  ->
    \+ tms_justification(TMS, Premises, Rule, rdf(S,P,O))
  ;
    \+ rdf(S, P, O, G)
  ),
  % Add to TMS.
  doyle_add_argument(TMS, Premises, Rule, rdf(S,P,O), J),

  % DEB
  if_debug(
    rdf_mat,
    (
      assert(recent_triple(S, P, O, G)),
      dcg_with_output_to(atom(Msg), materialize_message(TMS, J)),
      debug(rdf_mat, '~a', [Msg])
    )
  ),
  
  % Store the result.
  rdf_assert(S, P, O, G), !,
  
  % Look for more results...
  materialize(O1, TMS, G).
% Done!
materialize(O1, _TMS, _G):-
  % DEB
  option(entailment_regimes(ERs), O1, [rdf,rdfs]),
  dcg_with_output_to(atom(ER_Atom), list(pl_term, ERs)),
  if_debug(
    rdf_mat,
    (
      flag(deductions, N, 0),
      debug(rdf_mat, 'Added ~w deductions (regimes: ~w).', [N,ER_Atom])
    )
  ).

materialize_message(TMS, J) -->
  {flag(deductions, Id, Id + 1)},
  integer(Id),
  `: `,
  tms_print_justification([indent(0),lang(en)], TMS, J),
  nl.


%! rule(
%!   ?Regime:atom,
%!   ?Rule:atom,
%!   ?Premises:list(compound),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?Graph:atom
%! ) is nondet.

% All axioms can be deduced as if they are the outcome of a rule.
rule(Regime, axiom, [], S, P, O, _G):-
  axiom(Regime, S, P, O).
rule(Regime, Rule, Premises, S, P, O, G):-
  rdf_rule(Regime, Rule, Premises, S, P, O, G).
rule(Regime, Rule, Premises, S, P, O, G):-
  rdfs_rule(Regime, Rule, Premises, S, P, O, G).

%! start_materializer(
%!   +Options:list(nvpair),
%!   +Interval:positive_integer,
%!   +Graph:atom
%! ) is det.
% Performs a depth-one materialization step every N seconds.
%
% @arg Options A list of name-value pairs.
% @arg Interval The number of seconds between consecutive
%        materialization attempts.
% @arg Graph The atomic name of a graph
%        or uninstantiated (not restricted to a particular graph).
%
% @see Performs materialization steps using materialize/1.

start_materializer(O1, N, G):-
  default(30, N),
  intermittent_thread(materialize(O1, G), true, N, _Id, []),
  debug(rdf_mat, 'A materializer was started on graph ~w.', [G]).

