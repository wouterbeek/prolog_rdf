:- module(
  id_store_schema,
  [
    run/1 % +NumberOfThreads:positive_integer
  ]
).

/** <module> Identity store schema

@author Wouter Beek
@version 2015/10-2015/11
*/

:- use_module(library(counter)).
:- use_module(library(debug)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(service/ll_api)).

:- debug(ll(concurrent)).





run(Threads) :-
  run_on_term(Threads, owl:equivalentClass),
  run_on_term(Threads, owl:equivalentProperty),
  run_on_term(Threads, owl:sameAs).


run_on_term(Threads, X) :-
  run_on_tp(X, Threads, X, _, _),
  run_on_tp(X, Threads, _, _, X).


run_on_tp(Counter, Threads, S, P, O) :-
  create_counter(Counter),
  ll_concurrent(S, P, O, run_on_doc(Counter), Threads),
  delete_counter(Counter),
  msg_success("Done!").


run_on_doc(S, P, O, Counter, G) :-
  M = trp,
  % NONDET.
  ldf(S, P, O, G),
  increment_counter(Counter, Count),
  write(Count), tab, rdf_print_quad(M, S, P, O, G),
  qb(M, S, P, O, G),
  fail.
run_on_doc(_, _, _, _, _) :-
  increment_number_of_processed_jobs.
