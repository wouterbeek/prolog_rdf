:- module(
  id_store,
  [
    run/0,
    run/1 % ?NumberOfThreads:positive_integer
  ]
).

/** <module> Identity store

@author Wouter Beek
@version 2015/08, 2015/10-2015/11, 2016/01, 2016/03
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(default)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(llapi/llapi)).
:- use_module(library(os/io)).
:- use_module(library(os/thread_counter)).
:- use_module(library(print_ext)).

:- debug(llapi(concurrent)).
:- debug(llapi(id(doc))).





run:-
  run(_).


run(N):-
  call_to_nquads(
    'id.nt.gz',
    llapi_concurrent(_, owl:sameAs, _, store_id_doc, N)
  ).


store_id_doc(S, P, O, BPrefix, CT, Doc):-
  call_on_lod(
    S,
    P,
    O,
    Doc,
    store_id_triple(BPrefix, CT),
    []
  ), !,
  % DEBUG
  thread_counter(CT, NT),
  doc_name(Doc, Name),
  debug(llapi(id(doc)), "~D id-statements extracted from doc ~a.", [NT,Name]).
% DEBUG
store_id_doc(S, P, O, BPrefix, CT, Doc):-
  store_id_doc(S, P, O, BPrefix, CT, Doc).


store_id_triple(BPrefix, CT, S, P, O):-
  dcg_debug(llapi(id(stmt)), dcg_q_print_triple(S, P, O)),
  gen_ntriple(S, P, O), !.
% DEBUG
store_id_triple(_, _, S, P, O):-
  format(user_error, "Cannot write 〈~w,~w,~w〉~n", [S,P,O]).
