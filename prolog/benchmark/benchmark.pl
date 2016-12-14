:- module(
  benchmark,
  [
    benchmark_graph_query/5 % +Benchmark, ?GName, -G, ?QName, -QStr
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
:- use_module(library(q/q_dataset)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_rdf)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(yall)).

:- setting(
     q_query_dir,
     atom,
     '~/Data/query/',
     "Directory that holds the query files."
   ).





%! benchmark_graph_query(+Benchmark, +GName, -G, +QName, -QStr) is semidet.
%! benchmark_graph_query(+Benchmark, +GName, -G, -QName, -QStr) is nondet.
%! benchmark_graph_query(+Benchmark, -GName, -G, -QName, -QStr) is nondet.

benchmark_graph_query(Benchmark, GName, G, QName, QStr) :-
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
  call_on_stream(QFile, {QStr}/[In,Paths,Paths]>>read_stream_to_atom(In, QStr)).
