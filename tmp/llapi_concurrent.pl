:- module(
  ll_concurrent,
  [
    increment_number_of_processed_jobs/0,
    ll_concurrent/2, % :Goal_4, ?NumThreads
    ll_concurrent/5  % ?S, ?P, ?O, :Goal_4, ?NumThreads
  ]
).

/** <module> Concurrent processing of RDF data

@author Wouter Beek
@version 2015/08, 2015/10-2015/11, 2016/01
*/

:- use_module(library(debug)).
:- use_module(library(default)).
:- use_module(library(flag_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(print_ext)).
:- use_module(library(thread)).

:- meta_predicate
    concurrent_goal(1,+),
    lapi_concurrent(4,?),
    ll_concurrent(?,?,?,4,?),
    ll_concurrent0(?,?,?,4,+).

:- rdf_meta
   ll_concurrent(:,?),
   ll_concurrent(r,r,o,:,?),
   ll_concurrent0(r,r,o,:,+).

:- dynamic
    number_of_jobs/1,
    number_of_processed_jobs/1.





%! increment_number_of_processed_jobs is det.

increment_number_of_processed_jobs :-
  with_mutex(number_of_processed_jobs, (
    retract(number_of_processed_jobs(M)),
    N is M + 1,
    assert(number_of_processed_jobs(N)),
    number_of_jobs(O),
    P is N / O * 100,
    debug(ll(concurrent), "Processed ~D out of ~D jobs (~2f%).", [N,O,P])
  )).



%! ll_concurrent(:Goal_4, ?NumThreads) is det.
%! ll_concurrent(?S, ?P, ?O, :Goal_4, ?NumThreads) is det.

ll_concurrent(Goal_4, NumThreads):-
  ll_concurrent(_, _, _, Goal_4, NumThreads).


ll_concurrent(S, P, O, Goal_4, NumThreads):-
  defgoal(default_number_of_threads, NumThreads),
  tmp_set_prolog_flag(cpu_count, NumThreads,
    ll_concurrent0(S, P, O, Goal_4, NumThreads)
  ),
  msg_notification("Done running ~w!", [Goal_4]).

lodapi_concurrent0(S, P, O, Mod:Goal_4, NumThreads):-
  documents2(S, P, O, Locs),
  retractall(number_of_jobs(_)),
  length(Locs, M),
  assert(number_of_jobs(M)),
  retractall(number_of_processed_jobs(_)),
  assert(number_of_processed_jobs(0)),
  debug(
    ll(concurrent),
    "Going to process ~D jobs using ~D threads.",
    [M,NumThreads]
  ),
  Goal_4 =.. [Pred|Args],
  Goal_1 =.. [Pred,S,P,O|Args],
  concurrent_maplist(concurrent_goal(Mod:Goal_1), Locs),
  debug(
    ll(concurrent),
    "Done processing ~D jobs using ~D threads.",
    [M,NumThreads]
  ).

documents2(S, P, O, Locs):-
  findall(Locs, documents(S, P, O, Locs), Locss),
  append(Locss, Locs0),
  sort(Locs0, Locs).


%! concurrent_goal(:Goal_1, +Doc) is det.
%
% Concurrent maplist requires all threads to succeed.  We therefore
% catch all goals and assure that this calling goal never fails.

concurrent_goal(Goal_1, Doc):-
  catch(
    (
      call(Goal_1, Doc),
      % Keep statistics about the number of data documents
      % that have been processed.
      increment_number_of_processed_jobs
    ),
    E,
    format(
      user_error,
      "Exception ~q upon calling goal ~q for document ~a.~n",
      [E,Goal_1,Doc]
    )
  ), !.
concurrent_goal(Goal_1, Doc):-
  format(user_error, "Goal ~q failed for document ~a.~n", [Goal_1,Doc]).
