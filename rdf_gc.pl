:- module(
  rdf_gc,
  [
    rdf_graph_exclude_from_gc/1, % +Graph:atom
    rdf_graph_touch/1, % +Graph:atom
    rdf_touched_graph/3 % ?FirstTouch:float
                        % ?LastTouch:float
                        % ?Graph:atom
  ]
).

/** <module> RDF garbage collector

Graph-based garbage collection for RDF.

@author Wouter Beek
@version 2014/02, 2014/04-2014/05
*/

:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(thread_ext)).

:- use_module(plRdf(rdf_deb)).

%! rdf_graph_exlcuded_from_gc(?Graph:atom) is nondet.

:- dynamic(rdf_graph_exlcuded_from_gc/1).

%! rdf_touched_graph(+FirstTouch:float, +LastTouch:float, +Graph:atom) is semidet.
%! rdf_touched_graph(?FirstTouch:float, ?LastTouch:float, +Graph:atom) is semidet.
%! rdf_touched_graph(?FirstTouch:float, ?LastTouch:float, ?Graph:atom) is nondet.

:- dynamic(rdf_touched_graph/3).

:- initialization(init_rdf_gc_graph).



rdf_graph_exclude_from_gc(Graph):-
  rdf_graph_exlcuded_from_gc(Graph), !.
rdf_graph_exclude_from_gc(Graph):-
  with_mutex(
    rdf_gc,
    assert(rdf_graph_exlcuded_from_gc(Graph))
  ).


% Can't touch this!
rdf_graph_touch(Graph):-
  rdf_graph_exlcuded_from_gc(Graph).
rdf_graph_touch(Graph):-
  with_mutex(
    rdf_gc,
    rdf_graph_touch_sync(Graph)
  ).

rdf_graph_touch_sync(Graph):-
  retract(rdf_touched_graph(First, _, Graph)), !,
  get_time(Last),
  assert(rdf_touched_graph(First, Last, Graph)).
rdf_graph_touch_sync(Graph):-
  get_time(Now),
  assert(rdf_touched_graph(Now, Now, Graph)).



rdf_gc_by_graph:-
  rdf_statistics(triples(Triples)),
  rdf_gc_triples_by_graph(Triples).

rdf_gc_triples_by_graph(Triples):-
  % 10,000,000
  Triples =< 10000000, !.
rdf_gc_triples_by_graph(_):-
  findall(
    Time-Graph,
    rdf_touched_graph(_, Time, Graph),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, [Graph|_]),
  
  % Remove from administration.
  retract(rdf_touched_graph(_, Last, Graph)),
  
  % Assemble information to be displayed in debug message.
  duration(Last, Duration),
  rdf_statistics(triples_by_graph(Graph,Triples)),
  
  % Unload the graph and all of its contents.
  rdf_unload_graph_deb(Graph),
  
  % Display the debug message.
  format(
    user_output,
    '[-~:d triples] [~w dure] Unloaded graph ~w\n.',
    [Triples,Duration,Graph]
  ),
  flush_output(user_output),
  
  % See whether there is more work to do.
  rdf_gc_by_graph.

duration(Timestamp, Duration):-
  get_time(Now),
  Delta is Now - Timestamp,
  stamp_date_time(Delta, date(Y1,MM1,D1,H1,M1,S1,_,_,_), 'UTC'),
  Y2 is Y1 - 1970,
  MM2 is MM1 - 1,
  D2 is D1 - 1,
  format_time(atom(Duration), '%FT%T', date(Y2,MM2,D2,H1,M1,S1,0,-,-)).


init_rdf_gc_graph:-
  % Run 5 seconds.
  intermittent_thread(rdf_gc_by_graph, fail, 5, _, []).

