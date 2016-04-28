:- module(
  deref_hdt,
  [
    deref/1,
    overview_status/0,
    overview_wasted/0
  ]
).

/** <module> HDT dereference

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(hdt_ext)).
:- use_module(library(lists)).
:- use_module(library(memoization)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- meta_predicate
    hdt_goal0(1).

:- rdf_meta
   deref_hdt(r, r, o).





overview_status :-
  hdt_goal0(overview_status0).


overview_status0(Hdt) :-
  findall(N-Code, memo(deref_status0(Hdt, Code, N)), Pairs),
  desc_pairs(Pairs, SortedPairs),
  maplist(pair_list, SortedPairs, Rows),
  HeaderRow = ["Number of replies","HTTP status code"],
  dcg_with_output_to(user_output, dcg_table([head(HeaderRow)|Rows])).


deref_status0(Hdt, Code, N) :-
  distinct(Code, hdt(Hdt, _, deref:status, Code^^_)),
  rdf_aggregate_all(count, hdt(Hdt, _, deref:status, Code^^_), N).



iri_status(Hdt, Iri, Code) :-
  hdt(Hdt, Iri, deref:responses, L),
  hdt(Hdt, L, rdf:first, B),
  hdt(Hdt, B, deref:status, Code^^_).



iri_time(Hdt, Iri, Times, Time) :-
  hdt(Hdt, Iri, deref:responses, L),
  iri_times0(Hdt, L, Times),
  sum_list(Times, Time).


iri_times0(_, L, []) :-
  rdf_equal(L, rdf:nil), !.
iri_times0(Hdt, L1, [H|T]) :-
  hdt(Hdt, L1, rdf:first, B),
  hdt(Hdt, B, deref:time, H^^xsd:float),
  hdt(Hdt, L1, rdf:rest, L2),
  iri_times0(Hdt, L2, T).



overview_wasted :-
  hdt_goal0(overview_wasted0).


overview_wasted0(Hdt) :-
  flag(total, _, 0),
  flag(deref, _, 0),
  iri_wasted(Hdt, _, Total0, Deref0),
  Deref > 0,
  flag(total, Total, Total + Total0),
  flag(deref, Deref, Deref + Deref0),
  fail.
overview_wasted0(_) :-
  flag(total, Total, Total),
  flag(deref, Deref, Deref),
  format(user_output, "Total: ~w~nDeref: ~w~n", [Total,Deref]).



iri_wasted(Hdt, Iri, TotalTime, DerefTime) :-
  iri_time(Hdt, Iri, Times, TotalTime),
  calc_deref_time(Times, DerefTime).


calc_deref_time(Times, DerefTime) :-
  append(DerefTimes, [_], Times),
  sum_list(DerefTimes, DerefTime).





% API %

deref(S) :-
  hdt_goal0(deref0(S)).


deref0(S, Hdt) :-
  var(S), !,
  distinct(S, deref_subject0(Hdt, S)),
  deref(S).
deref(S, Hdt) :-
  findall(Triple, deref_triple0(Hdt, S, Triple), Triples),
  rdf_print_triples(Triples).


deref_subject0(Hdt, S) :-
  hdt_subject(Hdt, S),
  hdt(Hdt, S, deref:responses, _).


deref_triple0(Hdt, S, Triple) :-
  hdt(Hdt, S, P, O),
  (   Triple = rdf(S,P,O)
  ;   rdf_is_bnode(O),
      deref_triple0(Hdt, O, Triple)
  ).


hdt_goal0(Goal_1) :-
  hdt_prepare0(HdtFile),
  hdt_goal(HdtFile, Goal_1).


hdt_prepare0(HdtFile) :-
  hdt_file0(HdtFile),
  (   exists_file(HdtFile)
  ->  true
  ;   nt_file0(NTriplesFile),
      hdt_create_from_file(HdtFile, NTriplesFile, [])
  ).


hdt_file0('/ssd/lodlab/wouter/deref/deref.hdt').
nt_file0('/ssd/lodlab/wouter/deref/deref.nt').
