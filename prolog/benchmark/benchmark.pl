:- module(
  benchmark,
  [
    benchmark/1,               % ?Benchmark
    benchmark_dataset/2,       % ?Benchmark, ?Dataset
    benchmark_dataset_query/3, % ?Benchmark, ?Dataset, ?Query
    benchmark_dataset_query/4, % ?Benchmark, ?Dataset, ?Query, -Q
    run_benchmark/1,           % :Goals
    run_benchmark/4            % :Goals, ?Benchmark, ?Dataset, ?Query
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
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(yall)).

:- debug(benchmark).

:- meta_predicate
    run_benchmark(:),
    run_benchmark(:, ?, ?, ?),
    run_benchmark(:, ?, ?, ?, +),
    run_benchmark0(2, +, -, -).

:- setting(
     benchmark_dir,
     atom,
     '~/workspace/hdt-test/resource/',
     "The directory where benchmarks are stored."
   ).





%! benchmark(+Benchmark) is semidet.
%! benchmark(-Benchmark) is nondet.

benchmark(Benchmark) :-
  benchmark_directory0(Benchmark, _).



%! benchmark_dataset(+Benchmark, -Dataset) is nondet.
%! benchmark_dataset(-Benchmark, -Dataset) is nondet.

benchmark_dataset(Benchmark, Dataset) :-
  benchmark_directory0(Benchmark, Dir),
  benchmark_directory_dataset0(Benchmark, Dir, Dataset, _).



%! benchmark_dataset_query(+Benchmark, +Dataset, -Query) is nondet.
%! benchmark_dataset_query(+Benchmark, -Dataset, -Query) is nondet.
%! benchmark_dataset_query(-Benchmark, -Dataset, -Query) is nondet.

benchmark_dataset_query(Benchmark, Dataset, Query) :-
  benchmark_dataset_query0(Benchmark, Dataset, Query-_).


%! benchmark_dataset_query(+Benchmark, +Dataset, -Query, -Q) is nondet.
%! benchmark_dataset_query(+Benchmark, -Dataset, -Query, -Q) is nondet.
%! benchmark_dataset_query(-Benchmark, -Dataset, -Query, -Q) is nondet.

benchmark_dataset_query(Benchmark, Dataset, Query, Q) :-
  benchmark_dataset_query0(Benchmark, Dataset, Query-Path),
  call_on_stream(Path, {Q}/[In,Paths,Paths]>>read_stream_to_atom(In, Q)).



%! run_benchmark(:Goals) is det.
%! run_benchmark(:Goals, ?Benchmark, ?Dataset, ?Query) is nondet.

run_benchmark(Goals) :-
  run_benchmark(Goals, _, _, _).


run_benchmark(Goals, Benchmark, Dataset, Query) :-
  setup_call_cleanup(
    open('result.csv', write, Out),
    forall(
      benchmark_dataset_query(Benchmark, Dataset, Query),
      run_benchmark(Goals, Benchmark, Dataset, Query, Out)
    ),
    close(Out)
  ).


run_benchmark(Mod:Goals, Benchmark, Dataset, Query, Out) :-
  benchmark_dataset(Benchmark, Dataset),
  benchmark_dataset_query(Benchmark, Dataset, Query),
  debug(
    benchmark,
    "Benchmark ‘~a’, dataset ‘~a’, query ‘~a’.",
    [Benchmark,Dataset,Query]
  ),
  format(Out, "~a,~a,~a", [Benchmark,Dataset,Query]),
  forall(
    member(Goal, Goals),
    (
      run_benchmark0(Mod:Goal, Query, NumResults, Duration),
      format(Out, ",~d,~2f", [NumResults,Duration])
    )
  ),
  format(Out, "~n", []),
  flush_output(Out).


run_benchmark0(Goal_2, Query, NumResults, Duration) :-
  statistics(cputime, T1),
  flag(number_of_results, _, 0),
  forall(
    call(Goal_2, Query, _),
    flag(number_of_results, NumResults, NumResults + 1)
  ),
  statistics(cputime, T2),
  flag(number_of_results, NumResults, 0),
  Duration is T2 - T1.





% HELPERS %

%! benchmark_dataset_query0(+Benchmark, +Dataset, -Pair) is nondet.
%! benchmark_dataset_query0(+Benchmark, -Dataset, -Pair) is nondet.
%! benchmark_dataset_query0(-Benchmark, -Dataset, -Pair) is nondet.

benchmark_dataset_query0(Benchmark, Dataset, Pair) :-
  benchmark_directory0(Benchmark, Dir),
  benchmark_directory_dataset0(Benchmark, Dir, Dataset, File),
  rdf_reset_db,
  rdf_load_file(File),
  directory_file_path(Dir, query, Subdir),
  pair0(Subdir, sparql, Pair).



%! benchmark_directory0(+Benchmark, -Dir) is semidet.
%! benchmark_directory0(-Benchmark, -Dir) is nondet.

benchmark_directory0(Benchmark, Subdir) :-
  setting(benchmark_dir, Spec),
  expand_file_name(Spec, [Dir|_]),
  directory_subdirectory(Dir, Benchmark, Subdir).



%! benchmark_directory_dataset0(+Benchmark, +Dir, +Dataset, -Path) is semidet.
%! benchmark_directory_dataset0(+Benchmark, +Dir, -Dataset, -Path) is nondet.

benchmark_directory_dataset0(Benchmark, Dir, Dataset, Path) :-
  benchmark_directory0(Benchmark, Dir),
  directory_file_path(Dir, data, Subdir),
  pair0(Subdir, ttl, Dataset-Path).



pair0(Dir, Ext, Pair) :-
  % Sort by local file name.
  aggregate_all(
    set(File-Path),
    (
      directory_path(Dir, Path),
      file_name_extension(Local, Ext, Path),
      directory_file_path(Dir, File, Local)
    ),
    Pairs
  ),
  member(Pair, Pairs).
