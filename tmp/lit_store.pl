:- module(
  lit_store,
  [
    create_lit_store/1 % +NumberOfThreads:positive_integer
  ]
).

/** <module> Literal store

@author Wouter Beek
@version 2015/06
*/





create_lit_store(N):-
  setup_call_cleanup(
    storage(Out),
    tmp_set_prolog_flag(cpu_count, N, create_lit_store0(Out)),
    close(Out)
  ).

create_lit_store0(Out):-
  rdf_triple_concurrent(_, _, _, store_lit(Out)).

store_lit(S, P, O, Out, Md5):-
  ll_rdf(S, P, O, Md5),
  rdf_is_literal(O),
  md5_uri(Md5, G),
  with_output_to(Out, write_quadruple(S, P, O, G, _)),
  fail.
store_lit(_, _, _, _, _).

md5_uri(Md5, Uri):-
  atomic_concat('http://lodlaundromat.org/resource/', Md5, Uri).

storage(Out):-
  (   absolute_file_name(
        data(lit),
        File0,
        [access(read),file_errors(fail),file_type(ntriples)]
      )
  ->  delete_file(File0)
  ;   true
  ),
  absolute_file_name(data(lit), File, [access(write),file_type(ntriples)]),
  open(File, write, Out).
