:- module(
  benchmark,
  [
    benchmark_graph_query/5, % +Benchmark, ?GName, -G, ?QName, -Q
    run_benchmark/2,         % :Goals, +Benchmark
    run_benchmark/4          % :Goals, +Benchmark, ?GName, ?QName
  ]
).

/** <module> Benchmark

@author Wouter Beek
@version 2016/11
*/

:- use_module(library(atom_ext)).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io)).
:- use_module(library(q/q_dataset_db)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_rdf)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(yall)).

:- meta_predicate
    run_benchmark(:, +),
    run_benchmark(:, +, ?, ?),
    run_benchmark0(3, +, +, -, -),
    run_benchmark0(:, +, +, +, +, +, +).

:- setting(
     q_query_dir,
     atom,
     '~/Data/query/',
     "Directory that holds the query files."
   ).





%! benchmark_graph_query(+Benchmark, +GName, -G, +QName, -Q) is semidet.
%! benchmark_graph_query(+Benchmark, +GName, -G, -QName, -Q) is nondet.
%! benchmark_graph_query(+Benchmark, -GName, -G, -QName, -Q) is nondet.

benchmark_graph_query(Benchmark, GName, G, QName, Q) :-
  capitalize_atom(Benchmark, Benchmark0),
  q(hdt, D, rdfs:label, Benchmark0^^xsd:string),
  aggregate_all(
    set(GName-G),
    (
      q_dataset_graph(D, GName, G),
      GName \== meta
    ),
    GPairs
  ),
  member(GName-G, GPairs),
  setting(q_query_dir, QDir0),
  expand_file_name(QDir0, [QDir1|_]),
  directory_file_path(QDir1, Benchmark, QDir2),
  aggregate_all(
    set(QName-QFile),
    (
      directory_path(QDir2, QFile),
      file_name_extension(QFile0, sparql, QFile),
      directory_file_path(QDir2, QName, QFile0)
    ),
    QPairs
  ),
  member(QName-QFile, QPairs),
  call_on_stream(QFile, {Q}/[In,Paths,Paths]>>read_stream_to_atom(In, Q)).



%! run_benchmark(:Goals, +Benchmark) is det.
%! run_benchmark(:Goals, +Benchmark, ?GName, ?QName) is nondet.

run_benchmark(Goals, Benchmark) :-
  run_benchmark(Goals, Benchmark, _, _).


run_benchmark(Goals, Benchmark, GName, QName) :-
  setup_call_cleanup(
    open('result.csv', write, Out),
    forall(
      benchmark_graph_query(Benchmark, GName, G, QName, Q),
      run_benchmark0(Goals, Benchmark, GName, G, QName, Q, Out)
    ),
    close(Out)
  ).


run_benchmark0(Mod:Goals, Benchmark, GName, G, QName, Q, Out) :-
  format(Out, "~a,~a,~a", [Benchmark,GName,QName]),
  forall(
    member(Goal, Goals),
    (
      run_benchmark0(Mod:Goal, G, Q, NumResults, Duration),
      format(Out, ",~d,~2f", [NumResults,Duration])
    )
  ),
  format(Out, "~n", []),
  flush_output(Out).


run_benchmark0(Goal_3, G, Q, NumResults, Duration) :-
  statistics(cputime, T1),
  flag(number_of_results, _, 0),
  forall(
    call(Goal_3, G, Q, _),
    flag(number_of_results, NumResults, NumResults + 1)
  ),
  statistics(cputime, T2),
  flag(number_of_results, NumResults, 0),
  Duration is T2 - T1.
