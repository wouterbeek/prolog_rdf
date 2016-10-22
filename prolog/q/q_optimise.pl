:- module(
  q_optimise,
  [
    q_estimate_complexity/6,     % +M, ?S, ?P, ?O, ?G, -N
    q_object_branching_factor/4, % +M, +P, ?G, -N
    q_subject_branching_factor/4 % +M, +P, ?G, -N
  ]
).

/** <module> Quine: Optimisations

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(hdt/hdt_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   q_estimate_complexity(+, r, r, o, r, -).





%! q_estimate_complexity(+M, ?S, ?P, ?O, ?G, -N) is det.

q_estimate_complexity(hdt, S, P, O, G, N) :- !,
  hdt_estimate_complexity(S, P, O, G, N).
q_estimate_complexity(trp, S, P, O, _, N) :-
  rdf_estimate_complexity(S, P, O, N).



%! q_object_branching_factor(+M, +P, ?G, -Factor) is det.
%
%	The average number of atomic ground statements associated with each
%	unique value for the object-side of property P.

q_object_branching_factor(hdt, P, G, Factor) :-
  findall(
    N,
    (
      hdt_subject(S, G),
      hdt_estimate_complexity(S, P, _, G, N)
    ),
    Ns
  ),
  sum_list(Ns, SumNs),
  length(Ns, LenNs),
  (LenNs =:= 0 -> Factor = 0.0 ; Factor is SumNs / LenNs).
q_object_branching_factor(trp, P, _, Factor) :-
  rdf_predicate_property(P, rdf_object_branch_factor(Factor)).



%! q_subject_branching_factor(+M, +P, ?G, -Factor) is det.
%
%	The average number of atomic ground statements associated with each
%	unique value for the subject-side of property P.  If there are no
%	such atomic ground statements, then the value 0.0 is returned.

q_subject_branching_factor(hdt, P, G, Factor) :-
  findall(
    N,
    (
      hdt_subject(S, G),
      hdt_estimate_complexity(S, P, _, G, N)
    ),
    Ns
  ),
  sum_list(Ns, SumNs),
  length(Ns, LenNs),
  (LenNs =:= 0 -> Factor = 0.0 ; Factor is SumNs / LenNs).
q_subject_branching_factor(trp, P, _, Factor) :-
  rdf_predicate_property(P, rdf_subject_branch_factor(Factor)).
