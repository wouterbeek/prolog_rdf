:- module(
  benchmark,
  [
    benchmark_graph_query/4, % +Benchmark, ?GName, ?QName, -Q
    run_benchmark/1,         % :Goals
    run_benchmark/4          % :Goals, +Benchmark, ?GName, ?QName
  ]
).

/** <module> Benchmark

@author Wouter Beek
@version 2016/11
*/

:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io)).
:- use_module(library(q/q_dataset_db)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_rdf)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).
:- use_module(library(yall)).

:- debug(benchmark).

:- meta_predicate
    run_benchmark(:),
    run_benchmark(:, ?, ?, ?),
    run_benchmark0(2, +, -, -),
    run_benchmark0(:, +, ?, ?, +).

:- setting(
     q_query_dir,
     atom,
     '~/Data/query/',
     "Directory that holds the query files."
   ).





%! benchmark_graph_query(+Benchmark, +GName, +QName, -Q) is semidet.
%! benchmark_graph_query(+Benchmark, +GName, -QName, -Q) is nondet.
%! benchmark_graph_query(+Benchmark, -GName, -QName, -Q) is nondet.

benchmark_graph_query(Benchmark, GName, QName, Q) :-
  capitalize_string(Benchmark, Benchmark0),
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



%! run_benchmark(:Goals) is det.
%! run_benchmark(:Goals, +Benchmark, ?GName, ?QName) is nondet.

run_benchmark(Goals) :-
  run_benchmark(Goals, _, _, _).


run_benchmark(Goals, Benchmark, GName, QName) :-
  setup_call_cleanup(
    open('result.csv', write, Out),
    forall(
      benchmark_graph_query(Benchmark, GName, QName),
      run_benchmark0(Goals, Benchmark, GName, QName, Out)
    ),
    close(Out)
  ).


run_benchmark0(Goal_2, QName, NumResults, Duration) :-
  statistics(cputime, T1),
  flag(number_of_results, _, 0),
  forall(
    call(Goal_2, QName, _),
    flag(number_of_results, NumResults, NumResults + 1)
  ),
  statistics(cputime, T2),
  flag(number_of_results, NumResults, 0),
  Duration is T2 - T1.


run_benchmark0(Mod:Goals, Benchmark, GName, QName, Out) :-
  benchmark_graph(Benchmark, GName),
  benchmark_graph_query(Benchmark, GName, QName),
  debug(
    benchmark,
    "Benchmark ‘~a’, dataset ‘~a’, query ‘~a’.",
    [Benchmark,GName,QName]
  ),
  format(Out, "~a,~a,~a", [Benchmark,GName,QName]),
  forall(
    member(Goal, Goals),
    (
      run_benchmark0(Mod:Goal, QName, NumResults, Duration),
      format(Out, ",~d,~2f", [NumResults,Duration])
    )
  ),
  format(Out, "~n", []),
  flush_output(Out).
