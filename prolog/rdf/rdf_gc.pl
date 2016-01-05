:- module(
  rdf_gc,
  [
    rdf_graph_exclude_from_gc/1, % +G
    rdf_graph_touch/1,           % +G
    rdf_touched_graph/3          % ?FirstTouch:float
                                 % ?LastTouch:float
                                 % ?G
  ]
).

/** <module> RDF garbage collector

Graph-based garbage collection for RDF.

@author Wouter Beek
@version 2014/02, 2014/04-2014/05, 2016/01
*/

:- use_module(library(debug)).
:- use_module(library(os/thread_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_stats)).

%! rdf_graph_exlcuded_from_gc(?G) is nondet.

:- dynamic(rdf_graph_exlcuded_from_gc/1).

%! rdf_touched_graph(+FirstTouch:float, +LastTouch:float, +G) is semidet.
%! rdf_touched_graph(?FirstTouch:float, ?LastTouch:float, +G) is semidet.
%! rdf_touched_graph(?FirstTouch:float, ?LastTouch:float, ?G) is nondet.

:- dynamic(rdf_touched_graph/3).

:- initialization(init_rdf_gc_graph).





rdf_graph_exclude_from_gc(G) :-
  rdf_graph_exlcuded_from_gc(G), !.
rdf_graph_exclude_from_gc(G) :-
  with_mutex(rdf_gc, assert(rdf_graph_exlcuded_from_gc(G))).


rdf_graph_touch(G) :-
  rdf_graph_exlcuded_from_gc(G), !.
rdf_graph_touch(G) :-
  with_mutex(rdf_gc, rdf_graph_touch_sync(G)).

% The graph was previously touched.  Update timestamp.
rdf_graph_touch_sync(G) :-
  retract(rdf_touched_graph(First, _, G)), !,
  get_time(Last),
  assert(rdf_touched_graph(First, Last, G)).
% The graph is touched for the first time.
rdf_graph_touch_sync(G) :-
  get_time(Now),
  assert(rdf_touched_graph(Now, Now, G)).



rdf_gc_by_graph :-
  rdf_number_of_triples(N),
  (N > 1e6 -> rdf_gc_by_graph0 ; true).

rdf_gc_by_graph0 :-
  findall(Time-G, rdf_touched_graph(_, Time, G), Pairs),
  pairs_sorted_values(Pairs, @=<, [G|_]),
  retract(rdf_touched_graph(_, Last, G)),
  rdf_unload_graph(G),
  duration(Last, Duration),
  rdf_number_of_triples(G, N),
  debug(rdf_gc, "[-~:d] [~w] Unloaded graph ~w", [N,Duration,G]),
  rdf_gc_by_graph.

duration(Timestamp, Duration) :-
  get_time(Now),
  Delta is Now - Timestamp,
  stamp_date_time(Delta, date(Y1,Mo1,D1,H1,Mi1,S1,_,_,_), 'UTC'),
  Y2 is Y1 - 1970,
  Mo2 is Mo1 - 1,
  D2 is D1 - 1,
  format_time(atom(Duration), '%FT%T', date(Y2,Mo2,D2,H1,Mi1,S1,0,-,-)).


init_rdf_gc_graph:-
  % Run 5 seconds.
  intermittent_thread(rdf_gc_by_graph, fail, 5).
